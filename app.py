from flask import Flask, jsonify, make_response, request, redirect, url_for, render_template, session, flash, g
from werkzeug.security import generate_password_hash, check_password_hash
import ibm_db
import os
import subprocess
from pathlib import Path

# --- Rutas de COBOL ---
BASE_DIR = Path("/home/linux1/project")
COBOL_DIR = BASE_DIR / "cobol"
DATA_DIR = BASE_DIR / "data"

# --- Configuración Global (ahora con soporte para variables de entorno) ---
DB_CONN_STR = os.environ.get(
    "TESIS_DB_CONN_STR",
    "DATABASE=TESISDB;"
    "HOSTNAME=localhost;"
    "PORT=50000;"
    "PROTOCOL=TCPIP;"
    "UID=db2inst1;"
    "PWD=pass123;"
)

CODIGO_SECRETO_JEFE = os.environ.get("CODIGO_SECRETO_JEFE", "0125")

app = Flask(__name__)
# En producción: export FLASK_SECRET_KEY="algo-largo-y-random"
app.secret_key = os.environ.get("FLASK_SECRET_KEY", os.urandom(24))


# --- Funciones Auxiliares de DB ---
def execute_select_query(query):
    """
    Ejecuta un SELECT sin parámetros y devuelve (lista_diccionarios, status_code).
    IMPORTANTE: usar sólo con queries estáticos (sin datos del usuario).
    """
    conn = None
    try:
        conn = ibm_db.connect(DB_CONN_STR, "", "")
        stmt = ibm_db.exec_immediate(conn, query)
        results = []
        result = ibm_db.fetch_assoc(stmt)
        while result:
            results.append({k.lower(): v for k, v in result.items()})
            result = ibm_db.fetch_assoc(stmt)
        return results, 200
    except Exception as e:
        print(f"Error de DB en SELECT: {e}")
        return [], 500
    finally:
        if conn:
            ibm_db.close(conn)


# --- Rutas de la Aplicación ---

@app.route('/')
def home():
    """Ruta de bienvenida."""
    if session.get('loggedin'):
        rol = (session.get('rol') or "").strip()
        if rol == 'JEFE':
            return redirect(url_for('main_jefe'))
        elif rol == 'EMPLEADO':
            return redirect(url_for('main_empleado'))
    return redirect(url_for('show_login_form'))



@app.route('/main')
def main_screen():
    # Si no está logueado, lo mando al login
    if not session.get('loggedin'):
        return redirect(url_for('show_login_form'))

    rol = (session.get('rol') or "").strip()

    if rol == 'JEFE':
        # Podés renderizar directo:
        #return render_template('rol_jefe.html')
        # o redirigir a la ruta que ya tenés:
         return redirect(url_for('main_jefe'))

    elif rol == 'EMPLEADO':
        #return render_template('rol_empleado.html')
        # o:
         return redirect(url_for('main_empleado'))

    # Si por algún motivo el rol está raro:
    session.clear()
    flash("Rol desconocido. Iniciá sesión de nuevo.")
    return redirect(url_for('show_login_form'))



# --- Registro y Login ---

@app.route('/register', methods=['GET'])
def show_register_form():
    """Muestra el formulario HTML de registro."""
    return render_template('register.html')


@app.route('/register', methods=['POST'])
def register_user():
    """Procesa el formulario de registro (POST) con prepared statements."""
    data = request.form

    if data['contrasena'] != data['contrasena_confirm']:
        return "Error: Las contraseñas no coinciden. LMAO.", 400

    hashed_password = generate_password_hash(data['contrasena'], method='pbkdf2:sha256')

    if data['secret_key'] == CODIGO_SECRETO_JEFE:
        rol_usuario = 'JEFE'
    else:
        rol_usuario = 'EMPLEADO'

    estado_usuario = 'ACTIVO'

    conn = None
    stmt_persona = None
    stmt_usuario = None

    try:
        conn = ibm_db.connect(DB_CONN_STR, "", "")
        ibm_db.autocommit(conn, ibm_db.SQL_AUTOCOMMIT_OFF)

        # INSERT PERSONA
        q_persona = "INSERT INTO PERSONA (NOMBRE, EMAIL) VALUES (?, ?)"
        stmt_persona = ibm_db.prepare(conn, q_persona)
        ibm_db.execute(stmt_persona, (data['nombre'], data['email']))
        ibm_db.free_stmt(stmt_persona)
        stmt_persona = None

        # OBTENER ID_PERSONA
        q_get_id = "SELECT IDENTITY_VAL_LOCAL() FROM SYSIBM.SYSDUMMY1"
        stmt_id = ibm_db.exec_immediate(conn, q_get_id)
        result = ibm_db.fetch_row(stmt_id)
        if result:
            id_persona = int(ibm_db.result(stmt_id, 0))
        else:
            raise Exception("Falló la consulta de IDENTITY_VAL_LOCAL().")

        # INSERT USUARIO
        q_usuario = "INSERT INTO USUARIO (USUARIO, CONTRASENA, ROL, ID_PERSONA, ESTADO) VALUES (?, ?, ?, ?, ?)"
        stmt_usuario = ibm_db.prepare(conn, q_usuario)
        ibm_db.execute(stmt_usuario, (data['usuario'], hashed_password, rol_usuario, id_persona, estado_usuario))
        ibm_db.free_stmt(stmt_usuario)
        stmt_usuario = None

        ibm_db.commit(conn)
        return redirect(url_for('show_login_form')), 302

    except Exception as e:
        if conn:
            ibm_db.rollback(conn)

        error_details = str(e)

        if "-803" in error_details or "23505" in error_details:
            return "Error al registrar. El nombre de **usuario** o el **email** ya está en uso. ¡Sé más original!", 409

        if "KeyError" in error_details:
            return f"Error de programador: El formulario no envió el campo {error_details}. Asegúrate de que 'register.html' tenga 'nombre' y 'email'.", 500

        return f"Error al registrar. ¡Bug de DB! Detalles: {error_details}", 500

    finally:
        if stmt_persona:
            ibm_db.free_stmt(stmt_persona)
        if stmt_usuario:
            ibm_db.free_stmt(stmt_usuario)
        if conn:
            ibm_db.autocommit(conn, ibm_db.SQL_AUTOCOMMIT_ON)
            ibm_db.close(conn)


@app.route('/index', methods=['GET'])
def show_login_form():
    """Muestra el formulario HTML de login."""
    return render_template('index.html')


@app.route('/index', methods=['POST'])
def login_user():
    """Procesa el formulario de login (POST) y redirige por ROL."""
    data = request.form
    usuario_ingresado = data['usuario']
    contrasena_ingresada = data['contrasena']

    conn = None
    try:
        conn = ibm_db.connect(DB_CONN_STR, "", "")

        q_select = (
            "SELECT U.ID_USUARIO, U.CONTRASENA, U.ROL, U.ESTADO, P.NOMBRE "
            "FROM USUARIO AS U JOIN PERSONA AS P ON U.ID_PERSONA = P.ID_PERSONA "
            "WHERE U.USUARIO = ?"
        )

        stmt = ibm_db.prepare(conn, q_select)
        ibm_db.execute(stmt, (usuario_ingresado,))
        result = ibm_db.fetch_assoc(stmt)

        if result:
            hash_guardado = result['CONTRASENA'].strip()

            if check_password_hash(hash_guardado, contrasena_ingresada):

                if result['ESTADO'].strip() != 'ACTIVO':
                    ibm_db.close(conn)
                    return "Error: Usuario inactivo. Habla con el jefe.", 403

                session['loggedin'] = True
                session['id_usuario'] = result['ID_USUARIO']
                session['rol'] = result['ROL'].strip()
                session['usuario'] = usuario_ingresado
                session['nombre_persona'] = result['NOMBRE'].strip()

                ibm_db.close(conn)

                if session['rol'] == 'JEFE':
                    return redirect(url_for('main_jefe'))
                else:
                    return redirect(url_for('main_empleado'))
            else:
                ibm_db.close(conn)
                return "Error: Usuario o contraseña incorrectos. ¿Se te olvidó?", 401
        else:
            ibm_db.close(conn)
            return "Error: Usuario o contraseña incorrectos. ¿Se te olvidó?", 401

    except Exception as e:
        if conn:
            ibm_db.close(conn)
        return f"Error de conexión o DB. Detalles: {e}", 500


# --- Dashboards por Rol ---

@app.route('/dashboard/jefe')
def main_jefe():
    """Dashboard para Jefes: Acceso total."""
    if not session.get('loggedin'):
        return redirect(url_for('show_login_form'))

    if session.get('rol') != 'JEFE':
        return "Acceso Denegado. Esta área es solo para Jefes.", 403

    return render_template('rol_jefe.html')


@app.route('/rol_empleado')
def main_empleado():
    """Dashboard para Empleados: Enfocado en ventas."""
    if not session.get('loggedin'):
        return redirect(url_for('show_login_form'))

    if session.get('rol') not in ('EMPLEADO', 'JEFE'):
        return "Acceso Denegado.", 403

    return render_template('rol_empleado.html')


@app.route('/scanner')
def scanner_page():
    if 'loggedin' not in session or not session['loggedin']:
        return redirect(url_for('show_login_form'))

    # scanner lo pueden usar EMPLEADO y JEFE
    if session.get('rol') not in ('EMPLEADO', 'JEFE'):
        return "Acceso Denegado.", 403

    return render_template('scanner.html')


# --- Empleados ---

@app.route('/lista_empleados')
def lista_empleados():
    """Muestra la lista de empleados (solo para jefes)."""
    if not session.get('loggedin'):
        return redirect(url_for('show_login_form'))
    if session.get('rol') != 'JEFE':
        return "Acceso Denegado. Esta área es solo para Jefes.", 403

    return render_template('lista_empleados.html')


@app.route('/api/v1/empleados', methods=['GET'])
def get_employees():
    """API: lista completa de empleados/usuarios (solo JEFE)."""
    if not session.get('loggedin'):
        return jsonify({'error': 'no_auth'}), 401
    if session.get('rol') != 'JEFE':
        return jsonify({'error': 'forbidden'}), 403

    query = """
    SELECT P.ID_PERSONA, P.NOMBRE, P.TELEFONO, U.ESTADO
    FROM DB2INST1.PERSONA P
    INNER JOIN DB2INST1.USUARIO U ON P.ID_PERSONA = U.ID_PERSONA
    WHERE U.ROL = 'EMPLEADO'
    ORDER BY P.ID_PERSONA ASC
    """
    data, status_code = execute_select_query(query)

    if status_code == 200:
        return jsonify(data)
    else:
        return make_response(jsonify({'error': 'Error al obtener la lista de empleados.'}), 500)


@app.route('/registrar_empleados')
def registrar_empleados():
    if not session.get('loggedin'):
        return redirect(url_for('show_login_form'))
    if session.get('rol') != 'JEFE':
        return "Acceso Denegado. Esta área es solo para Jefes.", 403

    return render_template('registrar_empleados.html')


@app.route('/registrar_form', methods=['POST'])
def registrar_form():
    if request.method == 'POST':
        if not session.get('loggedin'):
            return redirect(url_for('show_login_form'))
        if session.get('rol') != 'JEFE':
            return "Acceso Denegado. Esta área es solo para Jefes.", 403

        nombre = request.form['nombre']
        usuario = request.form['usuario']
        email = request.form['email']
        contrasena = request.form['contrasena']
        telefono_str = request.form['telefono']

        hashed_contrasena = generate_password_hash(contrasena, method='pbkdf2:sha256')

        try:
            telefono_val = int(telefono_str) if telefono_str else None
        except ValueError:
            flash('Error: El teléfono debe ser un número válido.')
            return redirect(url_for('registrar_empleados'))

        conn = None
        stmt_persona = None
        stmt_usuario = None

        try:
            conn = ibm_db.connect(DB_CONN_STR, "", "")
            ibm_db.autocommit(conn, ibm_db.SQL_AUTOCOMMIT_OFF)

            # PERSONA
            sql_persona = "INSERT INTO DB2INST1.PERSONA (NOMBRE, EMAIL, TELEFONO) VALUES (?, ?, ?)"
            stmt_persona = ibm_db.prepare(conn, sql_persona)

            ibm_db.bind_param(stmt_persona, 1, nombre)
            ibm_db.bind_param(stmt_persona, 2, email)

            if telefono_val is None:
                ibm_db.bind_param(stmt_persona, 3, telefono_val, ibm_db.SQL_PARAM_INPUT, ibm_db.SQL_INTEGER)
            else:
                ibm_db.bind_param(stmt_persona, 3, telefono_val)

            ibm_db.execute(stmt_persona)
            ibm_db.free_stmt(stmt_persona)
            stmt_persona = None

            # ID_PERSONA
            q_get_id = "SELECT IDENTITY_VAL_LOCAL() FROM SYSIBM.SYSDUMMY1"
            stmt_id = ibm_db.exec_immediate(conn, q_get_id)
            result = ibm_db.fetch_row(stmt_id)
            if result:
                id_persona_nuevo = int(ibm_db.result(stmt_id, 0))
            else:
                raise Exception("Falló la consulta de IDENTITY_VAL_LOCAL().")

            # USUARIO (ROL y ESTADO: defaults de la DDL)
            sql_usuario = "INSERT INTO DB2INST1.USUARIO (USUARIO, CONTRASENA, ID_PERSONA) VALUES (?, ?, ?)"
            stmt_usuario = ibm_db.prepare(conn, sql_usuario)

            ibm_db.bind_param(stmt_usuario, 1, usuario)
            ibm_db.bind_param(stmt_usuario, 2, hashed_contrasena)
            ibm_db.bind_param(stmt_usuario, 3, id_persona_nuevo)

            ibm_db.execute(stmt_usuario)
            ibm_db.free_stmt(stmt_usuario)
            stmt_usuario = None

            ibm_db.commit(conn)

            flash('¡Empleado registrado exitosamente!', 'success')
            return redirect(url_for('lista_empleados'))

        except Exception as e:
            if conn:
                ibm_db.rollback(conn)

            error_msg = str(e)
            if "SQLCODE=-803" in error_msg:
                flash(f'Error: El nombre de usuario "{usuario}" ya existe. Intente con otro.', 'error')
            else:
                flash(f'Error inesperado en la base de datos: {error_msg}', 'error')

            return redirect(url_for('registrar_empleados'))

        finally:
            if stmt_persona:
                ibm_db.free_stmt(stmt_persona)
            if stmt_usuario:
                ibm_db.free_stmt(stmt_usuario)
            if conn:
                ibm_db.autocommit(conn, ibm_db.SQL_AUTOCOMMIT_ON)
                ibm_db.close(conn)

    return redirect(url_for('registrar_empleados'))


# --- API SCANNER & VENTA ---

from flask import jsonify, request, session

@app.route('/scanner/lookup', methods=['POST'])
def scanner_lookup():
    if not session.get('loggedin'):
        return jsonify({'status': 'error', 'mensaje': 'Usuario no autenticado. Inicie sesión.'}), 401

    codigo_barras = (request.form.get('codigo') or '').strip()
    if not codigo_barras:
        return jsonify({'status': 'error', 'mensaje': 'Falta el código de barras.'}), 400

    conn = None
    try:
        conn = ibm_db.connect(DB_CONN_STR, "", "")
        ibm_db.autocommit(conn, ibm_db.SQL_AUTOCOMMIT_OFF)

        # 1) Buscar producto existente
        q_lookup = """
            SELECT ID_PRODUCTO, NOMBRE, PRECIOVENTA, CATEGORIA, PORCENTAJEGANANCIA
            FROM PRODUCTO
            WHERE CODIGOBARRAS = ?
        """
        stmt_lookup = ibm_db.prepare(conn, q_lookup)
        ibm_db.execute(stmt_lookup, (codigo_barras,))
        producto = ibm_db.fetch_assoc(stmt_lookup)
        ibm_db.free_stmt(stmt_lookup)

        # 2) Si no existe: crear placeholder (para obtener ID_PRODUCTO)
        if not producto:
            q_insert = """
                INSERT INTO PRODUCTO (CODIGOBARRAS, NOMBRE, CATEGORIA, PRECIOVENTA, PORCENTAJEGANANCIA)
                VALUES (?, ?, ?, ?, ?)
            """
            stmt_ins = ibm_db.prepare(conn, q_insert)

            nombre_placeholder = f"NUEVO_{codigo_barras}"
            categoria_placeholder = "SIN CATEGORIA"
            precio_venta_placeholder = 0.0
            porc_gan_placeholder = 0.0

            try:
                ibm_db.execute(stmt_ins, (
                    codigo_barras,
                    nombre_placeholder,
                    categoria_placeholder,
                    precio_venta_placeholder,
                    porc_gan_placeholder
                ))
            except Exception as e:
                # Si justo lo creó otro dispositivo (duplicado), volvemos a leer
                msg = str(e)
                if ("-803" in msg) or ("23505" in msg):
                    ibm_db.free_stmt(stmt_ins)
                    stmt_lookup2 = ibm_db.prepare(conn, q_lookup)
                    ibm_db.execute(stmt_lookup2, (codigo_barras,))
                    producto = ibm_db.fetch_assoc(stmt_lookup2)
                    ibm_db.free_stmt(stmt_lookup2)
                else:
                    ibm_db.free_stmt(stmt_ins)
                    raise

            if not producto:
                q_get_id = "SELECT IDENTITY_VAL_LOCAL() FROM SYSIBM.SYSDUMMY1"
                stmt_id = ibm_db.exec_immediate(conn, q_get_id)
                if not ibm_db.fetch_row(stmt_id):
                    raise Exception("No se pudo obtener ID_PRODUCTO generado.")
                new_id = int(ibm_db.result(stmt_id, 0))

                ibm_db.commit(conn)
                return jsonify({
                    'status': 'new_registered',
                    'id_producto': new_id,
                    'codigo': codigo_barras,
                    'mensaje': 'Producto no registrado. Complete los datos y guarde la compra.'
                }), 200

        # 3) Producto encontrado (o recuperado tras -803)
        id_producto_real = int(producto['ID_PRODUCTO'])
        nombre_db = (producto.get('NOMBRE') or '').strip()
        precio_raw = producto.get('PRECIOVENTA')
        try:
            precio_db = float(precio_raw) if precio_raw is not None else 0.0
        except Exception:
            precio_db = 0.0
        categoria_db = (producto.get('CATEGORIA') or '').strip()

        producto_incompleto = (
            precio_db == 0.0 or
            nombre_db.startswith('NUEVO_') or
            categoria_db in ('', 'SIN CATEGORIA')
        )

        # Stock
        q_stock = "SELECT COALESCE(SUM(CANTIDAD), 0) AS STOCK_TOTAL FROM LOTE WHERE ID_PRODUCTO = ?"
        stmt_stock = ibm_db.prepare(conn, q_stock)
        ibm_db.execute(stmt_stock, (id_producto_real,))
        stock_data = ibm_db.fetch_assoc(stmt_stock)
        ibm_db.free_stmt(stmt_stock)
        stock_total = int(stock_data['STOCK_TOTAL']) if stock_data and stock_data.get('STOCK_TOTAL') is not None else 0

        # Alertas
        q_alerta = (
            "SELECT COUNT(A.ID_ALERTA) AS ALERTA_ACTIVA "
            "FROM ALERTA_VENCIMIENTO AS A "
            "JOIN LOTE AS L ON A.ID_LOTE = L.ID_LOTE "
            "WHERE L.ID_PRODUCTO = ? AND A.ESTADO = 'pendiente'"
        )
        stmt_alerta = ibm_db.prepare(conn, q_alerta)
        ibm_db.execute(stmt_alerta, (id_producto_real,))
        alerta_data = ibm_db.fetch_assoc(stmt_alerta)
        ibm_db.free_stmt(stmt_alerta)
        alerta_activa = bool(alerta_data and int(alerta_data.get('ALERTA_ACTIVA') or 0) > 0)

        ibm_db.commit(conn)

        return jsonify({
            'status': 'found',
            'id_producto': id_producto_real,
            'nombre': nombre_db,
            'categoria': categoria_db,
            'precio_venta': precio_db,
            'stock': stock_total,
            'ubicacion': 'A01',
            'alerta_vencimiento': alerta_activa,
            'incompleto': producto_incompleto
        }), 200

    except Exception as e:
        if conn:
            try:
                ibm_db.rollback(conn)
            except Exception:
                pass
        print(f"Error en /scanner/lookup: {e}")
        return jsonify({'status': 'error', 'mensaje': 'Error del servidor al consultar/crear el producto.'}), 500

    finally:
        if conn:
            try:
                ibm_db.autocommit(conn, ibm_db.SQL_AUTOCOMMIT_ON)
                ibm_db.close(conn)
            except Exception:
                pass



@app.route('/venta/confirmar', methods=['POST'])
def venta_confirmar():
    """Confirma la venta, resta stock (FEFO) y registra el COSTOTOTAL."""
    if not session.get('loggedin'):
        return jsonify({'status': 'error', 'mensaje': 'Usuario no autenticado. Inicie sesión de nuevo.'}), 401

    id_producto_str = request.form.get('id_producto')
    if not id_producto_str:
        return jsonify({'status': 'error', 'mensaje': 'Falta ID_PRODUCTO'}), 400

    try:
        id_producto = int(id_producto_str)
    except ValueError:
        return jsonify({'status': 'error', 'mensaje': 'ID_PRODUCTO inválido'}), 400

    id_usuario = session.get('id_usuario')
    conn = None

    try:
        conn = ibm_db.connect(DB_CONN_STR, "", "")
        ibm_db.autocommit(conn, ibm_db.SQL_AUTOCOMMIT_OFF)

        # PRECIOVENTA + LOTE FEFO (con PRECIOCOSTO)
        q_data = """
            SELECT P.PRECIOVENTA, L.ID_LOTE, L.PRECIOCOSTO
            FROM PRODUCTO AS P, LOTE AS L
            WHERE P.ID_PRODUCTO = ?
              AND P.ID_PRODUCTO = L.ID_PRODUCTO
              AND L.CANTIDAD > 0
            ORDER BY L.FECHAVENCIMIENTO ASC
            FETCH FIRST 1 ROW ONLY
        """
        stmt_data = ibm_db.prepare(conn, q_data)
        ibm_db.execute(stmt_data, (id_producto,))
        data_result = ibm_db.fetch_assoc(stmt_data)

        if not data_result:
            raise Exception("No hay stock disponible para vender (LOTE vacío o sin precio).")

        precio_venta = float(data_result['PRECIOVENTA'])
        precio_costo = float(data_result['PRECIOCOSTO'])
        id_lote_a_usar = data_result['ID_LOTE']

        # Descontar 1 unidad del lote
        q_update_lote = "UPDATE LOTE SET CANTIDAD = CANTIDAD - 1 WHERE ID_LOTE = ?"
        stmt_update = ibm_db.prepare(conn, q_update_lote)
        ibm_db.execute(stmt_update, (id_lote_a_usar,))
        ibm_db.free_stmt(stmt_update)

        # VENTA (cabecera) - preparado
        q_venta_cabecera = """
            INSERT INTO VENTA (ID_USUARIO, SUBTOTAL, TOTALVENTA)
            VALUES (?, ?, ?)
        """
        stmt_venta = ibm_db.prepare(conn, q_venta_cabecera)
        ibm_db.execute(stmt_venta, (id_usuario, precio_venta, precio_venta))
        ibm_db.free_stmt(stmt_venta)

        # ID_VENTA
        q_get_id = "SELECT IDENTITY_VAL_LOCAL() FROM SYSIBM.SYSDUMMY1"
        stmt_id = ibm_db.exec_immediate(conn, q_get_id)
        if not ibm_db.fetch_row(stmt_id):
            raise Exception("No se pudo obtener ID_VENTA generado.")
        id_venta = int(ibm_db.result(stmt_id, 0))

        # DETALLE_VENTA (incluye COSTOTOTAL) - preparado
        q_detalle = """
            INSERT INTO DETALLE_VENTA (ID_VENTA, ID_PRODUCTO, CANTIDAD, PRECIOVENTA, COSTOTOTAL)
            VALUES (?, ?, ?, ?, ?)
        """
        stmt_detalle = ibm_db.prepare(conn, q_detalle)
        ibm_db.execute(stmt_detalle, (id_venta, id_producto, 1, precio_venta, precio_costo))
        ibm_db.free_stmt(stmt_detalle)

        ibm_db.commit(conn)
        return jsonify({'status': 'success', 'mensaje': 'Venta registrada y stock descontado (FEFO).'})

    except Exception as e:
        if conn:
            ibm_db.rollback(conn)
        return jsonify({'status': 'error', 'mensaje': f"Error al confirmar: {str(e)}"}), 500

    finally:
        if conn:
            ibm_db.autocommit(conn, ibm_db.SQL_AUTOCOMMIT_ON)
            ibm_db.close(conn)


# --- Registrar Compras / Lotes ---

@app.route('/registrar_compra', methods=['GET', 'POST'])
def registrar_compra():
    if 'loggedin' not in session or not session['loggedin']:
        return redirect(url_for('show_login_form'))

    # empleados y jefes pueden registrar compras
    if session.get('rol') not in ('EMPLEADO', 'JEFE'):
        return "Acceso Denegado.", 403

    if request.method == 'POST':
        conn = None
        try:
            id_proveedor_str = request.form.get('id_proveedor')
            if not id_proveedor_str:
                flash('Error: Debe seleccionar un proveedor.', 'error')
                return redirect(url_for('registrar_compra'))

            id_proveedor = int(id_proveedor_str)

            product_indexes = set()
            for key in request.form.keys():
                if key.startswith('producto_') and key.endswith('_id'):
                    parts = key.split('_')
                    product_indexes.add(parts[1])

            if not product_indexes:
                flash('Error: No se agregó ningún producto a la compra.', 'error')
                return redirect(url_for('registrar_compra'))

            conn = ibm_db.connect(DB_CONN_STR, "", "")
            ibm_db.autocommit(conn, ibm_db.SQL_AUTOCOMMIT_OFF)

            for index in product_indexes:
                id_producto = int(request.form.get(f'producto_{index}_id'))
                nombre_nuevo = request.form.get(f'producto_{index}_nombre')
                categoria = request.form.get(f'producto_{index}_categoria') or None

                preciocosto_str = request.form.get(f'producto_{index}_preciocosto')
                preciocosto = float(preciocosto_str) if preciocosto_str else 0.0
                zona   = request.form.get(f'producto_{index}_ubicacion_zona') or ''
                codigo = request.form.get(f'producto_{index}_ubicacion_codigo') or ''
                if zona or codigo:
                    ubicacion = f"{zona.strip()}-{codigo.strip()}" if zona and codigo else zona or codigo
                else:
                    ubicacion = None

                precioventa_str = request.form.get(f'producto_{index}_precioventa')
                precioventa = float(precioventa_str) if precioventa_str else 0.0

                # 🔹 NUEVO: porcentaje de ganancia
                porc_ganancia_str = request.form.get(f'producto_{index}_porcganancia')
                porc_ganancia = float(porc_ganancia_str) if porc_ganancia_str else 0.0
                # si viene 0 o vacío, no sobreescribimos la BD
                porc_ganancia_val = porc_ganancia if porc_ganancia > 0 else None

                tipo_venta = request.form.get(f'producto_{index}_tipo_venta')
                cantidad_final = 0
                if tipo_venta == 'unidad':
                    cantidad_final = int(request.form.get(f'producto_{index}_cantidad_unidad', 0))
                elif tipo_venta == 'pack':
                    unidades = int(request.form.get(f'producto_{index}_unidades_pack', 0))
                    packs = int(request.form.get(f'producto_{index}_cantidad_pack', 0))
                    cantidad_final = unidades * packs

                if cantidad_final <= 0:
                    raise Exception(f'Producto ID {id_producto} tiene cantidad 0.')

                tiene_vencimiento = request.form.get(f'producto_{index}_tiene_vencimiento')
                fechavencimiento_val = None
                if tiene_vencimiento == 'si':
                    fechavencimiento_val = request.form.get(f'producto_{index}_fecha_vencimiento')
                    if not fechavencimiento_val:
                        raise Exception(f'Producto ID {id_producto} está marcado con vencimiento pero no tiene fecha.')

                # Actualizar PRODUCTO
                query_update_prod = """
                UPDATE PRODUCTO
                SET PRECIOVENTA = ?, NOMBRE = COALESCE(?, NOMBRE), CATEGORIA = COALESCE(?, CATEGORIA), PORCENTAJEGANANCIA = COALESCE(?, PORCENTAJEGANANCIA)
                WHERE ID_PRODUCTO = ?
                """
                stmt_update = ibm_db.prepare(conn, query_update_prod)
                ibm_db.execute(stmt_update, (precioventa, nombre_nuevo, categoria, porc_ganancia_val, id_producto))
                ibm_db.free_stmt(stmt_update)

                # Registrar LOTE
                query_insert_lote = """
                INSERT INTO LOTE (ID_PRODUCTO, ID_PROVEEDOR, CANTIDAD, PRECIOCOSTO, FECHAVENCIMIENTO, UBICACION)
                VALUES (?, ?, ?, ?, ?, ?)
                """
                stmt_lote = ibm_db.prepare(conn, query_insert_lote)

                print("--- DEBUGGING: INTENTANDO INSERTAR EN LOTE ---")
                print(f"ID_PRODUCTO: {id_producto} (Tipo: {type(id_producto)})")
                print(f"ID_PROVEEDOR: {id_proveedor} (Tipo: {type(id_proveedor)})")
                print(f"CATEGORIA: {categoria} (Tipo: {type(categoria)})")
                print(f"CANTIDAD: {cantidad_final} (Tipo: {type(cantidad_final)})")
                print(f"PRECIOCOSTO: {preciocosto} (Tipo: {type(preciocosto)})")
                print(f"FECHAVENCIMIENTO: {fechavencimiento_val} (Tipo: {type(fechavencimiento_val)})")
                print(f"UBICACION: {ubicacion} (Tipo: {type(ubicacion)})")
                print("---------------------------------------------")

                ibm_db.bind_param(stmt_lote, 1, id_producto)
                ibm_db.bind_param(stmt_lote, 2, id_proveedor)
                ibm_db.bind_param(stmt_lote, 3, cantidad_final)
                ibm_db.bind_param(stmt_lote, 4, preciocosto)
                ibm_db.bind_param(stmt_lote, 6, ubicacion)


                if fechavencimiento_val:
                    ibm_db.bind_param(stmt_lote, 5, fechavencimiento_val)
                else:
                    ibm_db.bind_param(stmt_lote, 5, fechavencimiento_val, ibm_db.SQL_PARAM_INPUT, ibm_db.SQL_DATE)

                ibm_db.execute(stmt_lote)
                ibm_db.free_stmt(stmt_lote)

            ibm_db.commit(conn)
            flash(f'¡Compra de {len(product_indexes)} productos registrada con éxito!', 'success')

        except Exception as e:
            if conn:
                ibm_db.rollback(conn)

            print("--- ERROR EN POST /registrar_compra ---")
            print(f"{e}")
            print("---------------------------------------")

            flash(f'Error al guardar la compra: {e}', 'error')

        finally:
            if conn:
                ibm_db.autocommit(conn, ibm_db.SQL_AUTOCOMMIT_ON)
                ibm_db.close(conn)

        return redirect(url_for('registrar_compra'))

    elif request.method == 'GET':
        proveedores = []
        conn = None
        try:
            conn = ibm_db.connect(DB_CONN_STR, "", "")
            query_prov = "SELECT ID_PROVEEDOR, RAZON_SOCIAL FROM PROVEEDOR ORDER BY RAZON_SOCIAL"
            stmt_prov = ibm_db.exec_immediate(conn, query_prov)
            prov = ibm_db.fetch_assoc(stmt_prov)
            while prov:
                proveedores.append(prov)
                prov = ibm_db.fetch_assoc(stmt_prov)
        except Exception as e:
            flash(f'Error al cargar la lista de proveedores: {e}', 'error')
        finally:
            if conn:
                ibm_db.close(conn)

        return render_template('registrar_compra.html', proveedores=proveedores)


# --- Proveedores ---

@app.route('/proveedores', methods=['GET'])
def gestionar_proveedores():
    """Muestra el formulario y la lista de proveedores (solo JEFE)."""
    if 'loggedin' not in session or not session['loggedin']:
        return redirect(url_for('show_login_form'))
    if session.get('rol') != 'JEFE':
        return "Acceso Denegado. Esta área es solo para Jefes.", 403

    proveedores, status_code = execute_select_query(
        "SELECT ID_PROVEEDOR, RAZON_SOCIAL, CUIT, TELEFONO FROM PROVEEDOR ORDER BY RAZON_SOCIAL"
    )

    if status_code != 200:
        flash('Error al cargar la lista de proveedores.', 'error')
        proveedores = []

    return render_template('proveedores.html', proveedores=proveedores)


@app.route('/proveedores', methods=['POST'])
def agregar_proveedor():
    """Procesa el formulario y guarda el nuevo proveedor (solo JEFE)."""
    if 'loggedin' not in session or not session['loggedin']:
        return redirect(url_for('show_login_form'))
    if session.get('rol') != 'JEFE':
        return "Acceso Denegado. Esta área es solo para Jefes.", 403

    data = request.form
    razon_social = data['razon_social']
    cuit = data['cuit']
    telefono = data['telefono']

    conn = None
    stmt = None
    try:
        conn = ibm_db.connect(DB_CONN_STR, "", "")
        query = "INSERT INTO PROVEEDOR (RAZON_SOCIAL, CUIT, TELEFONO) VALUES (?, ?, ?)"
        stmt = ibm_db.prepare(conn, query)
        ibm_db.bind_param(stmt, 1, razon_social)
        ibm_db.bind_param(stmt, 2, cuit)
        ibm_db.bind_param(stmt, 3, telefono)
        ibm_db.execute(stmt)
        flash('¡Proveedor registrado exitosamente!', 'success')

    except Exception as e:
        error_msg = str(e)
        if "SQLCODE=-803" in error_msg:
            flash(f'Error: El CUIT "{cuit}" ya está registrado.', 'error')
        else:
            flash(f'Error al registrar el proveedor: {error_msg}', 'error')
    finally:
        if stmt:
            ibm_db.free_stmt(stmt)
        if conn:
            ibm_db.close(conn)

    return redirect(url_for('gestionar_proveedores'))


# --- Página de Precios (JEFE) ---

@app.route('/precios')
def precios_page():
    if 'loggedin' not in session or not session['loggedin']:
        return redirect(url_for('show_login_form'))
    if session.get('rol') != 'JEFE':
        return "Acceso Denegado. Esta área es solo para Jefes.", 403

    query = """
    SELECT
        P.ID_PRODUCTO,
        P.NOMBRE,
        P.CATEGORIA,
        P.PRECIOVENTA,
        P.PORCENTAJEGANANCIA,
        P.CODIGOBARRAS,
        COALESCE(SUM(CASE WHEN L.CANTIDAD > 0 THEN L.CANTIDAD ELSE 0 END), 0) AS STOCK_TOTAL,
        COUNT(DISTINCT CASE WHEN L.CANTIDAD > 0 THEN L.ID_LOTE END) AS LOTES_CON_STOCK,
        CASE
            WHEN SUM(CASE WHEN L.CANTIDAD > 0 THEN L.CANTIDAD ELSE 0 END) > 0
            THEN SUM(L.CANTIDAD * L.PRECIOCOSTO)
                 / SUM(CASE WHEN L.CANTIDAD > 0 THEN L.CANTIDAD ELSE 0 END)
            ELSE NULL
        END AS COSTO_PROMEDIO,
        MIN(CASE WHEN L.CANTIDAD > 0 THEN L.FECHAVENCIMIENTO END) AS PROX_VENCIMIENTO,
        MIN(CASE WHEN L.CANTIDAD > 0 THEN L.UBICACION END) AS UBICACION_FEFO
    FROM PRODUCTO P
    LEFT JOIN LOTE L
           ON P.ID_PRODUCTO = L.ID_PRODUCTO
    GROUP BY
        P.ID_PRODUCTO,
        P.NOMBRE,
        P.CATEGORIA,
        P.PRECIOVENTA,
        P.PORCENTAJEGANANCIA,
        P.CODIGOBARRAS
    ORDER BY
        P.NOMBRE
    """

    productos, status_code = execute_select_query(query)

    if status_code != 200:
        flash('Error al cargar la lista de precios.', 'error')
        productos = []

    return render_template('precios.html', productos=productos)


@app.route('/api/productos/<int:id_producto>/lotes')
def api_lotes_por_producto(id_producto):
    """Detalle de lotes de un producto (login requerido)."""
    if not session.get('loggedin'):
        return jsonify({'status': 'error', 'mensaje': 'No autenticado'}), 401

    conn = None
    try:
        conn = ibm_db.connect(DB_CONN_STR, "", "")
        sql = """
        SELECT
            L.ID_LOTE,
            L.CANTIDAD,
            L.PRECIOCOSTO,
            L.FECHAVENCIMIENTO,
            L.UBICACION,
            L.FECHAINGRESO,
            L.ESTADO,
            PR.RAZON_SOCIAL AS PROVEEDOR
        FROM LOTE L
        JOIN PROVEEDOR PR
          ON PR.ID_PROVEEDOR = L.ID_PROVEEDOR
        WHERE L.ID_PRODUCTO = ?
        ORDER BY
            L.FECHAVENCIMIENTO ASC NULLS LAST,
            L.ID_LOTE ASC
        """
        stmt = ibm_db.prepare(conn, sql)
        ibm_db.execute(stmt, (id_producto,))

        lotes = []
        row = ibm_db.fetch_assoc(stmt)
        while row:
            lotes.append({k.lower(): v for k, v in row.items()})
            row = ibm_db.fetch_assoc(stmt)

        return jsonify({'status': 'ok', 'lotes': lotes})
    except Exception as e:
        print("Error en /api/productos/<id>/lotes:", e)
        return jsonify({'status': 'error', 'mensaje': str(e)}), 500
    finally:
        if conn:
            ibm_db.close(conn)


# --- Alertas de Vencimiento (JEFE) ---

@app.route('/alerta_vencimiento')
def alerta_vencimiento_page():
    if 'loggedin' not in session or not session['loggedin']:
        return redirect(url_for('show_login_form'))
    query = """
    SELECT
        A.ID_ALERTA,
        P.ID_PRODUCTO,
        P.NOMBRE,
        P.CATEGORIA,
        P.PRECIOVENTA         AS PRECIO_VENTA,
        L.FECHAVENCIMIENTO    AS FECHA_VENCIMIENTO,
        L.ID_LOTE             AS NUMERO_LOTE,
        L.CANTIDAD            AS STOCK_LOTE,
        (DAYS(L.FECHAVENCIMIENTO) - DAYS(CURRENT DATE)) AS DIAS_RESTANTES,
        CASE 
            WHEN L.FECHAVENCIMIENTO < CURRENT DATE THEN 'VENCIDO'
            WHEN L.FECHAVENCIMIENTO = CURRENT DATE THEN 'VENCE_HOY'
            WHEN L.FECHAVENCIMIENTO <= CURRENT DATE + 15 DAYS THEN 'PROXIMO_VENCER'
            ELSE 'INFO'
        END AS TIPO_ALERTA
    FROM ALERTA_VENCIMIENTO A
    JOIN LOTE L      ON L.ID_LOTE = A.ID_LOTE
    JOIN PRODUCTO P  ON P.ID_PRODUCTO = L.ID_PRODUCTO
    WHERE A.ESTADO = 'pendiente'
    ORDER BY L.FECHAVENCIMIENTO, P.NOMBRE
    """
    alertas, status_code = execute_select_query(query)
    if status_code != 200:
        flash('Error al cargar las alertas de vencimiento.', 'error')
        alertas = []

    return render_template('alerta_vencimiento.html', alertas=alertas)


# --- Productos (JEFE) ---

@app.route('/productos')
def producto_page():
    if 'loggedin' not in session or not session['loggedin']:
        return redirect(url_for('show_login_form'))
    if session.get('rol') != 'JEFE':
        return "Acceso Denegado. Esta área es solo para Jefes.", 403

    query = """
    SELECT
        P.ID_PRODUCTO,
        P.NOMBRE,
        P.CATEGORIA,
        P.PRECIOVENTA,
        MIN(CASE WHEN L.CANTIDAD > 0 THEN L.FECHAVENCIMIENTO ELSE NULL END) AS PROXIMO_VENCIMIENTO,
        COUNT(L.ID_LOTE) AS TOTAL_LOTES,
        SUM(L.CANTIDAD) AS STOCK_TOTAL
    FROM
        PRODUCTO P
    LEFT JOIN
        LOTE L ON P.ID_PRODUCTO = L.ID_PRODUCTO
    GROUP BY
        P.ID_PRODUCTO, P.NOMBRE, P.CATEGORIA, P.PRECIOVENTA
    ORDER BY
        P.NOMBRE
    """

    productos, status_code = execute_select_query(query)

    if status_code != 200:
        flash('Error al cargar la lista de productos.', 'error')
        productos = []

    return render_template('productos.html', productos=productos)


@app.route('/ajustes')
def ajustes_page():
    if 'loggedin' not in session or not session['loggedin']:
        return redirect(url_for('show_login_form'))

    if session.get('rol') != 'JEFE':
        return "Acceso Denegado. Esta área es solo para Jefes.", 403

    return render_template('ajustes.html')


# --- Módulo COBOL / Batch Costos & Alertas ---

def generar_costos_dat(conn):
    """Genera costos.dat para COBOL a partir de LOTE y PRODUCTO."""
    sql = """
    SELECT
        P.ID_PRODUCTO,
        P.PORCENTAJEGANANCIA,
        L.CANTIDAD,
        L.PRECIOCOSTO
    FROM LOTE L
    JOIN PRODUCTO P ON P.ID_PRODUCTO = L.ID_PRODUCTO
    WHERE L.CANTIDAD > 0
    ORDER BY P.ID_PRODUCTO, L.ID_LOTE
    """
    stmt = ibm_db.exec_immediate(conn, sql)

    costos_path = COBOL_DIR / "costos.dat"
    with costos_path.open("w", newline="\n") as f:
        row = ibm_db.fetch_assoc(stmt)
        while row:
            id_prod = int(row['ID_PRODUCTO'])
            porc_gan = float(row['PORCENTAJEGANANCIA'])
            cantidad = int(row['CANTIDAD'])
            precio_costo = float(row['PRECIOCOSTO'])

            line = f"{id_prod:09d}{porc_gan:06.2f}{cantidad:09d}{precio_costo:011.2f}"
            line = line.replace('.', ',')
            f.write(line + "\n")
            row = ibm_db.fetch_assoc(stmt)


def generar_vencimientos_dat(conn):
    """Genera vencimientos.dat para COBOL a partir de LOTE."""
    sql = """
    SELECT
        ID_LOTE
    FROM LOTE
    WHERE FECHAVENCIMIENTO IS NOT NULL
      AND FECHAVENCIMIENTO <= CURRENT DATE + 30 DAYS
      AND CANTIDAD > 0
    ORDER BY FECHAVENCIMIENTO, ID_LOTE
    """
    stmt = ibm_db.exec_immediate(conn, sql)

    venci_path = COBOL_DIR / "vencimientos.dat"
    with venci_path.open("w", newline="\n") as f:
        row = ibm_db.fetch_assoc(stmt)
        while row:
            id_lote = int(row["ID_LOTE"])
            line = f"{id_lote:09d}"
            f.write(line + "\n")
            row = ibm_db.fetch_assoc(stmt)


def ejecutar_batch_cobol():
    """Ejecuta el batch file para correr los programas COBOL."""
    try:
        result = subprocess.run(
            ["./run.sh"],
            cwd=str(COBOL_DIR),
            capture_output=True,
            text=True,
            check=False,
        )
        if result.returncode != 0:
            return False, f"batch COBOL ha devuelto RC={result.returncode}. stderr: {result.stderr}"
        else:
            return True, "batch COBOL ejecutado correctamente."
    except Exception as e:
        print(f"Error al generar archivos COBOL: {e}")
        return False, str(e)


def aplicar_historico_a_db(conn):
    """Lee historico.dat y actualiza PRODUCTO + PRECIO_HISTORICO."""
    hist_path = COBOL_DIR / "historico.dat"
    if not hist_path.exists():
        print("Archivo historico.dat no encontrado.")
        return

    with hist_path.open("r") as f:
        line = f.readline()
        while line:
            line = line.rstrip("\r\n")
            if line:
                line = line.replace("\x00", "")
                line = line.ljust(26)

                prod_txt = line[0:9].strip().replace("\x00", "")
                costo_txt = line[9:20].strip().replace("\x00", "")
                porcentaje_gan_txt = line[20:26].strip().replace("\x00", "")

                if ('.' not in costo_txt) and (',' not in costo_txt) and costo_txt:
                    costo_prom = int(costo_txt) / 100.0
                else:
                    costo_prom = float(costo_txt.replace(',', '.')) if costo_txt else 0.0

                if ('.' not in porcentaje_gan_txt) and (',' not in porcentaje_gan_txt) and porcentaje_gan_txt:
                    porcentaje_gan = int(porcentaje_gan_txt) / 100.0
                else:
                    porcentaje_gan = float(porcentaje_gan_txt.replace(',', '.')) if porcentaje_gan_txt else 0.0

                id_producto = int(prod_txt) if prod_txt else 0

                precio_venta = round(costo_prom * (1 + porcentaje_gan / 100), 2)

                sql_update = """
                UPDATE PRODUCTO
                SET PRECIOVENTA = ?
                WHERE ID_PRODUCTO = ?
                """
                stmt_update = ibm_db.prepare(conn, sql_update)
                ibm_db.execute(stmt_update, (precio_venta, id_producto))

                sql_insert = """
                INSERT INTO PRECIO_HISTORICO (ID_PRODUCTO, FECHA, PRECIOCOMPRA, PORCENTAJEGANANCIA, ORIGEN)
                VALUES (?, CURRENT DATE, ?, ?, 'BATCH COBOL')
                """
                stmt_insert = ibm_db.prepare(conn, sql_insert)
                ibm_db.execute(stmt_insert, (id_producto, costo_prom, porcentaje_gan))
            line = f.readline()


def aplicar_alertas_vencimiento(conn):
    """Lee alertas.dat y crea registros en ALERTA_VENCIMIENTO."""
    alertas_path = COBOL_DIR / "alertas.dat"
    if not alertas_path.exists():
        print("Archivo alertas.dat no encontrado.")
        return

    with alertas_path.open("r") as f:
        line = f.readline()
        while line:
            line = line.strip()
            if line:
                id_lote = int(line)
                sql_insertar = """
                INSERT INTO ALERTA_VENCIMIENTO (ID_LOTE, ESTADO)
                VALUES (?, 'pendiente')
                """
                try:
                    stmt_insertar = ibm_db.prepare(conn, sql_insertar)
                    ibm_db.execute(stmt_insertar, (id_lote,))
                except Exception as e:
                    print(f"Error al insertar alerta para LOTE {id_lote}: {e}")
            line = f.readline()


def run_cobol_batch():
    """Flujo completo: genera .dat, corre COBOL y aplica cambios a la DB."""
    conn = None
    try:
        conn = ibm_db.connect(DB_CONN_STR, "", "")
        ibm_db.autocommit(conn, ibm_db.SQL_AUTOCOMMIT_OFF)

        generar_costos_dat(conn)
        generar_vencimientos_dat(conn)

        ibm_db.commit(conn)
        ibm_db.close(conn)
        conn = None

        ok, mensaje = ejecutar_batch_cobol()
        print(mensaje)
        if not ok:
            return False, mensaje

        conn = ibm_db.connect(DB_CONN_STR, "", "")
        ibm_db.autocommit(conn, ibm_db.SQL_AUTOCOMMIT_OFF)
        aplicar_historico_a_db(conn)
        aplicar_alertas_vencimiento(conn)
        ibm_db.commit(conn)
        return True, "Proceso COBOL completado y DB actualizada."

    except Exception as e:
        if conn:
            ibm_db.rollback(conn)
        return False, f"Error en run_cobol_batch: {e}"
    finally:
        if conn:
            ibm_db.autocommit(conn, ibm_db.SQL_AUTOCOMMIT_ON)
            ibm_db.close(conn)


@app.route('/batch/costos', methods=['POST'])
def batch_costos():
    if not session.get('loggedin'):
        return redirect(url_for('show_login_form'))
    if session.get('rol') != 'JEFE':
        return "Acceso Denegado. Esta área es solo para Jefes.", 403

    ok, mensaje = run_cobol_batch()
    category = 'success' if ok else 'error'
    flash(mensaje, category)
    return redirect(url_for('precios_page'))


@app.route('/test_batch')
def test_batch():
    """Endpoint de prueba del batch COBOL (solo JEFE, para evitar que sea público)."""
    if not session.get('loggedin'):
        return redirect(url_for('show_login_form'))
    if session.get('rol') != 'JEFE':
        return "Acceso Denegado. Esta área es solo para Jefes.", 403

    ok, msg = run_cobol_batch()
    return f"OK={ok} - {msg}"

# --- Página Ofertas (JEFE) ---
@app.route('/ofertas')
def ofertas_page():
    """Listado de ofertas guardadas (solo JEFE)."""
    if not session.get('loggedin'):
        return redirect(url_for('show_login_form'))
    if session.get('rol') != 'JEFE':
        return "Acceso Denegado. Esta área es solo para Jefes.", 403

    conn = None
    ofertas = []
    try:
        conn = ibm_db.connect(DB_CONN_STR, "", "")

        sql = """
        SELECT
            O.ID_OFERTA,
            O.NOMBRE,
            O.PRECIO_COMBO,
            O.GANANCIA_PORC,
            COUNT(OD.ID_PRODUCTO)                 AS CANT_ITEMS,
            COALESCE(SUM(OD.CANTIDAD), 0)         AS UNIDADES_TOTALES
        FROM OFERTA O
        LEFT JOIN OFERTA_DETALLE OD
               ON O.ID_OFERTA = OD.ID_OFERTA
        GROUP BY
            O.ID_OFERTA,
            O.NOMBRE,
            O.PRECIO_COMBO,
            O.GANANCIA_PORC
        ORDER BY
            O.ID_OFERTA DESC
        """
        stmt = ibm_db.exec_immediate(conn, sql)
        row = ibm_db.fetch_assoc(stmt)
        while row:
            ofertas.append({k.lower(): v for k, v in row.items()})
            row = ibm_db.fetch_assoc(stmt)

    except Exception as e:
        print("Error en /ofertas:", e)
        flash(f"Error al cargar las ofertas: {e}", "error")
    finally:
        if conn:
            ibm_db.close(conn)

    return render_template('ofertas.html', ofertas=ofertas)

# --- OFERTAS PARA PANEL EXPANDIBLE ---
@app.route('/api/ofertas/<int:id_oferta>/detalle')
def api_oferta_detalle(id_oferta):
    """Detalle de productos que componen una oferta (solo JEFE)."""
    if not session.get('loggedin'):
        return jsonify({'status': 'error', 'mensaje': 'No autenticado'}), 401
    if session.get('rol') != 'JEFE':
        return jsonify({'status': 'error', 'mensaje': 'Solo JEFE'}), 403

    conn = None
    try:
        conn = ibm_db.connect(DB_CONN_STR, "", "")

        sql = """
        SELECT
            OD.ID_PRODUCTO,
            OD.CANTIDAD,
            P.NOMBRE,
            P.CATEGORIA,
            P.PRECIOVENTA
        FROM OFERTA_DETALLE OD
        JOIN PRODUCTO P
          ON P.ID_PRODUCTO = OD.ID_PRODUCTO
        WHERE OD.ID_OFERTA = ?
        ORDER BY P.NOMBRE
        """
        stmt = ibm_db.prepare(conn, sql)
        ibm_db.execute(stmt, (id_oferta,))

        items = []
        row = ibm_db.fetch_assoc(stmt)
        while row:
            items.append({k.lower(): v for k, v in row.items()})
            row = ibm_db.fetch_assoc(stmt)

        return jsonify({'status': 'ok', 'items': items})

    except Exception as e:
        print("Error en /api/ofertas/<id>/detalle:", e)
        return jsonify({'status': 'error', 'mensaje': str(e)}), 500
    finally:
        if conn:
            ibm_db.close(conn)
# --- DEPOSITO ---
@app.route('/deposito')
def deposito_page():
    """Vista del depósito: lotes físicos ordenados por ubicación (FEFO)."""
    if not session.get('loggedin'):
        return redirect(url_for('show_login_form'))

    conn = None
    lotes = []
    try:
        conn = ibm_db.connect(DB_CONN_STR, "", "")

        sql = """
        SELECT
            L.ID_LOTE,
            L.UBICACION,
            L.ID_PRODUCTO,
            P.NOMBRE,
            P.CATEGORIA,
            L.CANTIDAD,
            L.FECHAVENCIMIENTO,
            L.FECHAINGRESO,
            L.ESTADO,
            (DAYS(L.FECHAVENCIMIENTO) - DAYS(CURRENT DATE)) AS DIAS_RESTANTES
        FROM LOTE L
        JOIN PRODUCTO P
          ON P.ID_PRODUCTO = L.ID_PRODUCTO
        WHERE L.CANTIDAD > 0
        ORDER BY
            COALESCE(L.UBICACION, 'ZZZ') ASC,
            L.FECHAVENCIMIENTO ASC NULLS LAST,
            L.ID_LOTE ASC
        """
        stmt = ibm_db.exec_immediate(conn, sql)
        row = ibm_db.fetch_assoc(stmt)
        while row:
            lotes.append({k.lower(): v for k, v in row.items()})
            row = ibm_db.fetch_assoc(stmt)

    except Exception as e:
        print("Error en /deposito:", e)
        flash(f"Error al cargar el depósito: {e}", "error")
        lotes = []
    finally:
        if conn:
            ibm_db.close(conn)

    return render_template('deposito.html', lotes=lotes)


# --- Ofertas inteligentes ---

@app.route('/api/oferta_candidatos/<int:id_alerta>')
def api_oferta_candidatos(id_alerta):
    """
    Devuelve productos candidatos para combinar en una oferta.
    Usa:
      - categoría del producto en alerta
      - reglas en REGLA_COMBO_CATEGORIA (categorías relacionadas)
    """
    if not session.get('loggedin'):
        return jsonify({'error': 'no_auth'}), 401

    conn = None
    try:
        conn = ibm_db.connect(DB_CONN_STR, "", "")

        # 1) Producto base de la alerta
        q_alerta = """
        SELECT
            P.ID_PRODUCTO,
            P.CATEGORIA
        FROM ALERTA_VENCIMIENTO A
        JOIN LOTE L      ON L.ID_LOTE = A.ID_LOTE
        JOIN PRODUCTO P  ON P.ID_PRODUCTO = L.ID_PRODUCTO
        WHERE A.ID_ALERTA = ?
        """
        stmt_a = ibm_db.prepare(conn, q_alerta)
        ibm_db.execute(stmt_a, (id_alerta,))
        row = ibm_db.fetch_assoc(stmt_a)
        ibm_db.free_stmt(stmt_a)

        if not row:
            return jsonify({'error': 'not_found'}), 404

        id_producto_base = row['ID_PRODUCTO']
        categoria_base   = row['CATEGORIA'].strip() if row['CATEGORIA'] else None

        if not categoria_base:
            return jsonify({'status': 'ok', 'candidatos': []})

        # 2) Leer categorías relacionadas desde REGLA_COMBO_CATEGORIA
        q_reglas = """
        SELECT CATEGORIA_RELACIONADA
        FROM REGLA_COMBO_CATEGORIA
        WHERE CATEGORIA_BASE = ?
        """
        stmt_r = ibm_db.prepare(conn, q_reglas)
        ibm_db.execute(stmt_r, (categoria_base,))
        categorias = [categoria_base]  # siempre incluir la propia categoría

        reg = ibm_db.fetch_assoc(stmt_r)
        while reg:
            cat_rel = reg['CATEGORIA_RELACIONADA']
            if cat_rel:
                cat_rel = cat_rel.strip()
                if cat_rel and cat_rel not in categorias:
                    categorias.append(cat_rel)
            reg = ibm_db.fetch_assoc(stmt_r)
        ibm_db.free_stmt(stmt_r)

        # Si solo está la categoría base y no hay reglas, igual seguirá
        # sugiriendo productos de la misma categoría (como antes).

        # 3) Armar consulta dinámica con IN (?, ?, ?)
        placeholders = ",".join(["?"] * len(categorias))

        q_candidatos = f"""
        SELECT
            P.ID_PRODUCTO,
            P.NOMBRE,
            P.CATEGORIA,
            P.PRECIOVENTA                  AS PRECIO_VENTA,
            COALESCE(SUM(L.CANTIDAD), 0)   AS STOCK_TOTAL,
            MIN(L.FECHAVENCIMIENTO)        AS PROXIMO_VENC
        FROM PRODUCTO P
        LEFT JOIN LOTE L
               ON L.ID_PRODUCTO = P.ID_PRODUCTO
              AND L.CANTIDAD > 0
        WHERE P.CATEGORIA IN ({placeholders})
          AND P.ID_PRODUCTO <> ?
        GROUP BY
            P.ID_PRODUCTO, P.NOMBRE, P.CATEGORIA, P.PRECIOVENTA
        ORDER BY
            PROXIMO_VENC ASC NULLS LAST,
            STOCK_TOTAL DESC
        FETCH FIRST 8 ROWS ONLY
        """

        params = categorias + [id_producto_base]

        stmt_c = ibm_db.prepare(conn, q_candidatos)
        ibm_db.execute(stmt_c, tuple(params))

        candidatos = []
        r = ibm_db.fetch_assoc(stmt_c)
        while r:
            c = {k.lower(): v for k, v in r.items()}
            candidatos.append(c)
            r = ibm_db.fetch_assoc(stmt_c)
        ibm_db.free_stmt(stmt_c)

        return jsonify({'status': 'ok', 'candidatos': candidatos})

    except Exception as e:
        if conn:
            ibm_db.close(conn)
        print(f"Error en /api/oferta_candidatos: {e}")
        return jsonify({'error': 'server_error', 'detail': str(e)}), 500
    finally:
        if conn:
            ibm_db.close(conn)



@app.route('/ofertas/guardar', methods=['POST'])
def guardar_oferta():
    if not session.get('loggedin'):
        return jsonify({'status': 'error', 'message': 'No autenticado'}), 401
    if session.get('rol') != 'JEFE':
        return jsonify({'status': 'error', 'message': 'Sólo JEFE puede guardar ofertas'}), 403

    try:
        payload = request.get_json(force=True)
    except Exception as e:
        return jsonify({'status': 'error', 'message': f'JSON inválido: {e}'}), 400

    nombre = payload.get('nombre')
    ganancia = payload.get('ganancia')
    precio_combo = payload.get('precio_combo')
    productos = payload.get('productos')

    if not nombre or not isinstance(productos, list) or len(productos) == 0:
        return jsonify({
            'status': 'error',
            'message': 'Datos incompletos: nombre y productos son obligatorios.'
        }), 400

    conn = None
    try:
        conn = ibm_db.connect(DB_CONN_STR, "", "")
        ibm_db.autocommit(conn, ibm_db.SQL_AUTOCOMMIT_OFF)

        sql_oferta = """
            INSERT INTO OFERTA (NOMBRE, PRECIO_COMBO, GANANCIA_PORC)
            VALUES (?, ?, ?)
        """
        stmt_oferta = ibm_db.prepare(conn, sql_oferta)

        p_val = float(precio_combo) if precio_combo is not None else 0.0
        g_val = float(ganancia) if ganancia is not None else 0.0

        ibm_db.execute(stmt_oferta, (nombre, p_val, g_val))
        ibm_db.free_stmt(stmt_oferta)

        q_get_id = "SELECT IDENTITY_VAL_LOCAL() FROM SYSIBM.SYSDUMMY1"
        stmt_id = ibm_db.exec_immediate(conn, q_get_id)
        if not ibm_db.fetch_row(stmt_id):
            raise Exception("No se pudo obtener ID_OFERTA generado.")
        id_oferta = int(ibm_db.result(stmt_id, 0))

        sql_det = """
            INSERT INTO OFERTA_DETALLE (ID_OFERTA, ID_PRODUCTO, CANTIDAD)
            VALUES (?, ?, ?)
        """
        stmt_det = ibm_db.prepare(conn, sql_det)

        for item in productos:
            id_prod = int(item.get('id_producto'))
            cant = int(item.get('cantidad', 1))
            ibm_db.execute(stmt_det, (id_oferta, id_prod, cant))

        ibm_db.free_stmt(stmt_det)

        ibm_db.commit(conn)

        return jsonify({'status': 'ok', 'id_oferta': id_oferta}), 200

    except Exception as e:
        if conn:
            ibm_db.rollback(conn)
        print('Error en /ofertas/guardar:', e)
        return jsonify({'status': 'error', 'message': str(e)}), 500
    finally:
        if conn:
            ibm_db.autocommit(conn, ibm_db.SQL_AUTOCOMMIT_ON)
            ibm_db.close(conn)


# --- API Alertas resumen/lista (para el ícono de campana) ---

@app.route('/api/alertas/resumen')
def api_alertas_resumen():
    if not session.get('loggedin'):
        return jsonify({'status': 'error', 'mensaje': 'No autenticado'}), 401

    conn = None
    try:
        conn = ibm_db.connect(DB_CONN_STR, "", "")
        sql = """
        SELECT
            SUM(CASE WHEN L.FECHAVENCIMIENTO < CURRENT DATE THEN 1 ELSE 0 END) AS VENCIDAS,
            SUM(CASE WHEN L.FECHAVENCIMIENTO = CURRENT DATE THEN 1 ELSE 0 END) AS HOY,
            SUM(CASE WHEN L.FECHAVENCIMIENTO > CURRENT DATE
                     AND L.FECHAVENCIMIENTO <= CURRENT DATE + 15 DAYS THEN 1 ELSE 0 END) AS PROXIMAS
        FROM ALERTA_VENCIMIENTO A
        JOIN LOTE L ON L.ID_LOTE = A.ID_LOTE
        WHERE A.ESTADO = 'pendiente'
        """
        stmt = ibm_db.exec_immediate(conn, sql)
        row = ibm_db.fetch_assoc(stmt) or {}
        vencidas = int(row.get('VENCIDAS') or 0)
        hoy = int(row.get('HOY') or 0)
        proximas = int(row.get('PROXIMAS') or 0)
        total = vencidas + hoy + proximas

        return jsonify({
            'status': 'ok',
            'total': total,
            'vencidas': vencidas,
            'hoy': hoy,
            'proximas': proximas
        })
    except Exception as e:
        print('Error en /api/alertas/resumen:', e)
        return jsonify({'status': 'error', 'mensaje': str(e)}), 500
    finally:
        if conn:
            ibm_db.close(conn)


@app.route('/api/alertas/lista')
def api_alertas_lista():
    if not session.get('loggedin'):
        return jsonify({'status': 'error', 'mensaje': 'No autenticado'}), 401

    conn = None
    try:
        conn = ibm_db.connect(DB_CONN_STR, "", "")
        sql = """
        SELECT
            A.ID_ALERTA,
            P.NOMBRE,
            P.CATEGORIA,
            L.FECHAVENCIMIENTO    AS FECHA_VENCIMIENTO,
            (DAYS(L.FECHAVENCIMIENTO) - DAYS(CURRENT DATE)) AS DIAS_RESTANTES
        FROM ALERTA_VENCIMIENTO A
        JOIN LOTE L      ON L.ID_LOTE = A.ID_LOTE
        JOIN PRODUCTO P  ON P.ID_PRODUCTO = L.ID_PRODUCTO
        WHERE A.ESTADO = 'pendiente'
        ORDER BY L.FECHAVENCIMIENTO, P.NOMBRE
        FETCH FIRST 10 ROWS ONLY
        """
        stmt = ibm_db.exec_immediate(conn, sql)

        alertas = []
        row = ibm_db.fetch_assoc(stmt)
        while row:
            alertas.append({k.lower(): v for k, v in row.items()})
            row = ibm_db.fetch_assoc(stmt)

        return jsonify({'status': 'ok', 'alertas': alertas})
    except Exception as e:
        print('Error en /api/alertas/lista:', e)
        return jsonify({'status': 'error', 'mensaje': str(e)}), 500
    finally:
        if conn:
            ibm_db.close(conn)


# --- Punto de Entrada ---
if __name__ == '__main__':
    # En producción, usar debug=0 y un reverse proxy (nginx) delante si quisieras HTTPS.
    app.run(host='0.0.0.0', port=8080, debug=False)
