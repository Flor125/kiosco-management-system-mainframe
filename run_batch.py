import ibm_db
import subprocess
import os
import sys
from pathlib import Path
from datetime import datetime, timedelta

BASE_DIR = Path("/home/linux1/project")
COBOL_DIR = BASE_DIR / "cobol"

COSTOS_PATH = COBOL_DIR / "costos.dat"
VENC_PATH   = COBOL_DIR / "vencimientos.dat"
HIST_PATH   = COBOL_DIR / "historico.dat"
ALERT_PATH  = COBOL_DIR / "alertas.dat"

# --- 1. Configuración de Conexión ---
# (Usa tus credenciales de TESISDB)
# OJO: Asegúrate que el hostname sea el nombre de tu contenedor RHEL
# si ejecutas esto desde el host Ubuntu. Si lo ejecutas DENTRO
# del contenedor RHEL, usa 'localhost'.
DB_CON_STRING = "DATABASE=TESISDB;HOSTNAME=localhost;PORT=50000;PROTOCOL=TCPIP;UID=db2inst1;PWD=pass123;"

def conectar_db():
    try:
        conn = ibm_db.connect(DB_CON_STRING, "", "")
        print("Conectado a TESISDB.")
        return conn
    except Exception as e:
        print(f"Error conectando a DB: {e}", file=sys.stderr)
        return None

def extractor_costos(conn):
    print("Iniciando Extractor de Costos...")
    # Esta query hace el JOIN y ORDENA.
    # El ORDER BY p.ID_PRODUCTO es MANDATORIO para el COBOL.
    SQL = """
        SELECT p.ID_PRODUCTO, p.PORCENTAJEGANANCIA, l.CANTIDAD, l.PRECIOCOSTO
        FROM LOTE l
        JOIN PRODUCTO p ON l.ID_PRODUCTO = p.ID_PRODUCTO
        WHERE l.CANTIDAD > 0
        ORDER BY p.ID_PRODUCTO
    """
    
    try:
        stmt = ibm_db.exec_immediate(conn, SQL)
        
        # Preparamos el archivo para COBOL (Formato TEXTO)
        with open("costos.dat", "w", encoding="utf-8") as f:
            result = ibm_db.fetch_tuple(stmt)
            while result:
                # Extraemos y limpiamos
                prod_id = str(result[0]).strip().ljust(9)
                # Formato "25.50".ljust(6)
                ganancia = f"{float(result[1]):.2f}".ljust(6)
                cantidad = str(result[2]).strip().ljust(9)
                # Formato "150.75".ljust(11)
                costo = f"{float(result[3]):.2f}".ljust(11)
                
                # Escribimos la línea fija (debe coincidir con FD de COBOL)
                f.write(f"{prod_id}{ganancia}{cantidad}{costo}\n")
                
                result = ibm_db.fetch_tuple(stmt)
                
        print("Extractor de Costos: 'costos.dat' creado.")
        return True
        
    except Exception as e:
        print(f"Error en Extractor de Costos: {e}", file=sys.stderr)
        return False

def extractor_vencimientos(conn):
    print("Iniciando Extractor de Vencimientos...")
    # Query de lotes a vencer en 15 días
    SQL = """
        SELECT ID_LOTE
        FROM LOTE
        WHERE FECHAVENCIMIENTO <= (CURRENT DATE + 15 DAYS)
          AND CANTIDAD > 0
          AND ESTADO = 'en_stock'
    """
    
    try:
        stmt = ibm_db.exec_immediate(conn, SQL)
        
        with open("vencimientos.dat", "w", encoding="utf-8") as f:
            result = ibm_db.fetch_tuple(stmt)
            while result:
                lote_id = str(result[0]).strip().ljust(9)
                f.write(f"{lote_id}\n")
                result = ibm_db.fetch_tuple(stmt)
                
        print("Extractor de Vencimientos: 'vencimientos.dat' creado.")
        return True
        
    except Exception as e:
        print(f"Error en Extractor de Vencimientos: {e}", file=sys.stderr)
        return False

def llamar_a_cobol():
    print("Iniciando Transformador (COBOL)...")
    try:
        # Asegúrate de que 'batchcosto' tenga permisos (chmod +x batchcosto)
        resultado = subprocess.run(
            ["./batchcosto"], 
            capture_output=True, 
            text=True, 
            timeout=300 # 5 minutos
        )
        
        if resultado.returncode == 0:
            print("COBOL ejecutado con éxito.")
            print(f"Salida COBOL:\n{resultado.stdout}")
            return True
        else:
            print(f"ERROR en COBOL (return code: {resultado.returncode})", file=sys.stderr)
            print(f"STDERR: {resultado.stderr}", file=sys.stderr)
            print(f"STDOUT: {resultado.stdout}", file=sys.stderr)
            return False
            
    except Exception as e:
        print(f"Error al llamar a subprocess: {e}", file=sys.stderr)
        return False

def cargador(conn):
    print("Iniciando Cargador...")
    
    try:
        # 1. Cargar Histórico
        with open("historico.dat", "r") as f:
            SQL_INSERT_HIST = """
                INSERT INTO PRECIO_HISTORICO
                  ( ID_PRODUCTO, FECHA, PRECIOCOMPRA, PORCENTAJEGANANCIA, ORIGEN )
                VALUES
                  ( ?, CURRENT DATE, ?, ?, 'COBOL_BATCH' )
            """
            stmt_hist = ibm_db.prepare(conn, SQL_INSERT_HIST)
            
            for linea in f:
                if not linea.strip():
                    continue
                
                # Parseamos el archivo de texto
                prod_id = int(linea[0:9].strip())
                costo_prom = float(linea[9:20].strip().replace(",", "."))
                ganancia = float(linea[20:26].strip().replace(",", "."))
                
                # Insertamos con parámetros
                ibm_db.execute(stmt_hist, (prod_id, costo_prom, ganancia))

        print("Cargador: 'historico.dat' procesado.")

        # 2. Cargar Alertas
        with open("alertas.dat", "r") as f:
            SQL_INSERT_ALERTA = """
                INSERT INTO ALERTA_VENCIMIENTO (ID_LOTE)
                VALUES (?)
            """
            stmt_alerta = ibm_db.prepare(conn, SQL_INSERT_ALERTA)
            
            for linea in f:
                if not linea.strip():
                    continue
                
                lote_id = int(linea[0:9].strip())
                
                try:
                    ibm_db.execute(stmt_alerta, (lote_id,))
                except Exception as e:
                    # Ignoramos error de "primary key" duplicada (-803)
                    if "-803" not in str(e):
                        raise e # Relanzamos si es otro error

        print("Cargador: 'alertas.dat' procesado.")

        # Si todo salió bien, comiteamos
        ibm_db.commit(conn)
        print("Cargador: Datos insertados y comiteados en DB2.")
        return True
        
    except Exception as e:
        print(f"Error en Cargador: {e}", file=sys.stderr)
        ibm_db.rollback(conn)
        print("Cargador: ROLLBACK ejecutado.", file=sys.stderr)
        return False

def limpiar_archivos():
    # Limpia para la próxima ejecución
    print("Limpiando archivos temporales...")
    for f in ["costos.dat", "vencimientos.dat", "historico.dat", "alertas.dat"]:
        if os.path.exists(f):
            os.remove(f)
    print("Archivos temporales limpiados.")


# --- MAIN: El Orquestador ---
def main():
    conn = conectar_db()
    if conn is None:
        sys.exit(1) # Salir con código de error

    limpiar_archivos() # Limpiamos por si quedó basura

    # 1. EXTRACT
    if not extractor_costos(conn) or not extractor_vencimientos(conn):
        print("Fallo en Extractor. Abortando.", file=sys.stderr)
        ibm_db.close(conn)
        sys.exit(1)

    # 2. TRANSFORM
    if not llamar_a_cobol():
        print("Fallo en COBOL. Abortando.", file=sys.stderr)
        ibm_db.close(conn)
        sys.exit(1)

    # 3. LOAD
    if not cargador(conn):
        print("Fallo en Cargador. Abortando.", file=sys.stderr)
        ibm_db.close(conn)
        sys.exit(1)

    print("\n--- Proceso Batch COMPLETADO con éxito ---")
    
    # Cerrar conexión y limpiar
    ibm_db.close(conn)
    limpiar_archivos()

if __name__ == "__main__":
    main()