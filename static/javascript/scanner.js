// static/javascript/scanner.js

// --- CONFIG DESDE EL HTML ---
const SCANNER_CFG = window.SCANNER_CFG || {};
const LOOKUP_URL   = SCANNER_CFG.lookupUrl;
const CONFIRMAR_URL = SCANNER_CFG.confirmarUrl;
const LOGIN_URL     = SCANNER_CFG.loginUrl;
const ALERTAS_URL   = SCANNER_CFG.alertasUrl;
const PRECIOS_URL  = SCANNER_CFG.preciosUrl; // registrar_compra
const recentReads = new Map(); // code -> {count, lastTs}
const CONSENSUS_MIN = 3;       // 2 o 3 según tolerancia
const CONSENSUS_MS  = 1200;    // ventana de tiempo


// Por si algo no está configurado
if (!LOOKUP_URL || !CONFIRMAR_URL || !LOGIN_URL) {
    console.error('SCANNER_CFG incompleto. Revisa window.SCANNER_CFG en scanner.html');
}

// --- ESTADO GLOBAL DEL ESCÁNER ---
let scannerIsActive = false;

let scanLock = false;
let lastCode = '';
let lastTs = 0;

function isValidEAN13(code) {
  if (!/^\d{13}$/.test(code)) return false;
  let sum = 0;
  for (let i = 0; i < 12; i++) {
    const n = code.charCodeAt(i) - 48;
    sum += (i % 2 === 0) ? n : n * 3;
  }
  const check = (10 - (sum % 10)) % 10;
  return check === (code.charCodeAt(12) - 48);
}


// --- 1. BÚSQUEDA / LOOKUP ---
async function buscarProducto(codigo) {
    const messageArea   = document.getElementById('message-area');
    const resultsArea   = document.getElementById('results-area');
    const alertaBanner  = document.getElementById('alertaVencimientoBanner');

    messageArea.textContent = 'Buscando producto en DB2...';
    messageArea.style.color = '#000';
    if (resultsArea) resultsArea.style.display = 'none';
    if (alertaBanner) alertaBanner.style.display = 'none';

    if (!codigo) {
        messageArea.textContent = '¡No se detectó un código!';
        return;
    }

    try {
        const response = await fetch(LOOKUP_URL, {
            method: 'POST',
            headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
            body: `codigo=${encodeURIComponent(codigo)}`
        });

        if (response.status === 401) {
            alert("Su sesión ha expirado o no ha iniciado sesión.");
            window.location.href = LOGIN_URL;
            return;
        }

        if (response.status === 404) {
  // NO registrado: caso normal
  messageArea.textContent = `Producto no registrado: ${codigo}.`;
  messageArea.style.color = '#d32f2f';

  // Panel visible pero sin inventar datos
  if (resultsArea) resultsArea.style.display = 'block';

  const nombreEl = document.getElementById('nombre_producto');
  const precioEl = document.getElementById('precio_venta');
  const stockEl  = document.getElementById('stock_total');
  const ubicEl   = document.getElementById('ubicacion');
  const idProdEl = document.getElementById('id_producto_display');
  const actions  = document.getElementById('transaction-actions');

  if (nombreEl) nombreEl.textContent = 'NO REGISTRADO';
  if (precioEl) precioEl.textContent = '—';
  if (stockEl)  stockEl.textContent  = '—';
  if (ubicEl)   ubicEl.textContent   = '—';
  if (idProdEl) idProdEl.textContent = '';           // evita “()”
  if (actions)  actions.style.display = 'none';

  // Preguntar y redirigir (una sola vez por lectura)
  setTimeout(() => {
    const ok = confirm(`Producto no registrado: ${codigo}\n\n¿Querés registrarlo ahora?`);
    if (ok) {
      const base = PRECIOS_URL || '/registrar_compra';
      window.location.href = `${base}?barcode=${encodeURIComponent(codigo)}`;
    } else {
      // si canceló, permitir volver a escanear desde botón
      // (no reinicio automático para no molestar)
    }
  }, 50);

  return;
}


if (!response.ok) {
    const errorTxt = await response.text().catch(() => '');
    messageArea.textContent = `ERROR ${response.status}: servidor falló al consultar.`;
    messageArea.style.color = '#d32f2f';
    console.error("Respuesta inesperada:", errorTxt.substring(0, 400));
    return;
    }


        const data = await response.json();

        if (data.status === 'error') {
            messageArea.textContent = `ERROR: ${data.mensaje}`;
            return;
        }

        if (resultsArea) resultsArea.style.display = 'block';
        const idProdEl = document.getElementById('id_producto_display');
        if (idProdEl) idProdEl.textContent = data.id_producto;

        if (data.status === 'new_registered') {
            // Producto NUEVO
            messageArea.textContent = `Producto no registrado: ${codigo}.`;
  messageArea.style.color = '#d32f2f';

  if (resultsArea) resultsArea.style.display = 'block';

  const nombreEl = document.getElementById('nombre_producto');
  const precioEl = document.getElementById('precio_venta');
  const stockEl  = document.getElementById('stock_total');
  const ubicEl   = document.getElementById('ubicacion');
  const idProdEl = document.getElementById('id_producto_display');
  const actions  = document.getElementById('transaction-actions');

  if (nombreEl) nombreEl.textContent = 'NO REGISTRADO';
  if (precioEl) precioEl.textContent = '—';
  if (stockEl)  stockEl.textContent  = '—';
  if (ubicEl)   ubicEl.textContent   = '—';
  if (idProdEl) idProdEl.textContent = '';
  if (actions)  actions.style.display = 'none';

  setTimeout(() => {
    const ok = confirm(`Producto no registrado: ${codigo}\n\n¿Querés registrarlo ahora?`);
    if (ok) {
      const base = PRECIOS_URL || '/registrar_compra';
      window.location.href = `${base}?barcode=${encodeURIComponent(codigo)}`;
    }
  }, 50);

  return;

        } else if (data.status === 'found') {
    // Siempre mostramos los datos básicos
    document.getElementById('nombre_producto').textContent = data.nombre;
    document.getElementById('precio_venta').textContent   = data.precio_venta.toFixed(2);
    document.getElementById('stock_total').textContent    = data.stock;
    document.getElementById('ubicacion').textContent      = data.ubicacion;

    // 👇 Si es producto “incompleto”, NO dejamos vender
    if (data.incompleto) {
        messageArea.textContent = '⚠ Producto sin registrar correctamente. Configura nombre, categoría y precio antes de vender.';
        messageArea.style.color = '#d32f2f';
        document.getElementById('transaction-actions').style.display = 'none';

        // Opcional: sugerir ir a la pantalla de precios
        if (window.SCANNER_CFG && window.SCANNER_CFG.preciosUrl) {
            const ir = confirm('Este código tiene precio 0 o está como NUEVO_. ¿Querés ir a la pantalla de Precios para completarlo?');
            if (ir) {
                window.location.href = window.SCANNER_CFG.preciosUrl;
            }
        }
    } else {
        // Producto bien configurado → flujo normal de venta
        messageArea.textContent = 'Producto encontrado. ¿El cliente compra?';
        messageArea.style.color = 'green';
        document.getElementById('transaction-actions').style.display = 'block';
    }
}


    } catch (error) {
        messageArea.textContent = `Fallo de conexión (CATCH): ${error}`;
        console.error(error);
    }
}

// Para el botón de búsqueda manual
function buscarProductoManual() {
    const input = document.getElementById('codigo_barras');
    if (!input) return;
    const codigo = input.value.trim();
    buscarProducto(codigo);
}

// --- 2. CONFIRMAR VENTA ---
async function confirmarVenta() {
    const idProdEl    = document.getElementById('id_producto_display');
    const messageArea = document.getElementById('message-area');
    const resultsArea = document.getElementById('results-area');
    const inputCodigo = document.getElementById('codigo_barras');

    if (!idProdEl) return;
    const id_producto = idProdEl.textContent;

    messageArea.textContent = 'Registrando venta y descontando stock...';

    try {
        const response = await fetch(CONFIRMAR_URL, {
            method: 'POST',
            headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
            body: `id_producto=${encodeURIComponent(id_producto)}`
        });

        if (response.status === 401) {
            alert("Su sesión ha expirado o no ha iniciado sesión.");
            window.location.href = LOGIN_URL;
            return;
        }

        if (!response.ok) {
            const errorHtml = await response.text();
            messageArea.textContent = `ERROR ${response.status}: El servidor devolvió un error inesperado al confirmar.`;
            console.error("Respuesta HTML/Texto inesperada:", errorHtml.substring(0, 200));
            return;
        }

        const data = await response.json();

        if (data.status === 'error') {
            messageArea.textContent = `ERROR AL VENDER: ${data.mensaje}`;
        } else {
            messageArea.textContent = `✅ ${data.mensaje} ¡Stock actualizado!`;
            if (resultsArea) resultsArea.style.display = 'none';
            if (inputCodigo) inputCodigo.value = '';
        }

    } catch (error) {
        messageArea.textContent = `Fallo de conexión (CATCH): ${error}`;
        console.error(error);
    }
}

// --- 3. SOLO CONSULTA ---
function soloConsulta() {
    const messageArea = document.getElementById('message-area');
    const resultsArea = document.getElementById('results-area');
    const inputCodigo = document.getElementById('codigo_barras');

    if (messageArea) {
        messageArea.textContent = 'Consulta de precio realizada. Stock NO modificado.';
    }
    if (resultsArea) resultsArea.style.display = 'none';
    if (inputCodigo) inputCodigo.value = '';
}

// --- 4. LÓGICA DE ESCANEO (QUAGGA) ---
let handlersBound = false;

function startScanner() {
  if (typeof Quagga === 'undefined') {
    alert('Quagga no está cargado.');
    return;
  }

  Quagga.init({
    inputStream: {
      name: "Live",
      type: "LiveStream",
      target: document.querySelector('#interactive'),
      constraints: {
        facingMode: "environment",
        width: { ideal: 1280 },
        height: { ideal: 720 }
      }
    },
    locate: true,
    decoder: {
      readers: ["ean_reader", "ean_8_reader", "code_128_reader", "upc_reader"]
    }
  }, function(err) {
    const messageArea = document.getElementById('message-area');
    if (err) {
      console.error(err);
      if (messageArea) {
        messageArea.textContent = 'Error al iniciar la cámara.';
        messageArea.style.color = 'red';
      }
      alert("Error al iniciar la cámara. ¿Tienes permisos?");
      return;
    }

    // Bind handlers SOLO UNA VEZ
    if (!handlersBound) {
      handlersBound = true;

      Quagga.onDetected(function(result) {
        const raw = result && result.codeResult && result.codeResult.code
          ? String(result.codeResult.code).trim()
          : '';
        if (!raw) return;

        const code = raw.replace(/\D/g, '');
        if (!code) return;

        // Si es EAN-13, validar checksum (filtra lecturas inventadas)
        if (code.length === 13 && !isValidEAN13(code)) return;

        const now = Date.now();

        // limpiar lecturas viejas
        for (const [k, v] of recentReads) {
          if (now - v.lastTs > CONSENSUS_MS) recentReads.delete(k);
        }

        const v = recentReads.get(code) || { count: 0, lastTs: 0 };
        v.count += 1;
        v.lastTs = now;
        recentReads.set(code, v);

        // esperar consenso
        if (v.count < CONSENSUS_MIN) return;

        // aceptar
        if (scanLock) return;
        scanLock = true;

        try { Quagga.stop(); } catch (e) {}
        scannerIsActive = false;

        const input = document.getElementById('codigo_barras');
        if (input) input.value = code;

        buscarProducto(code).finally(() => {
          recentReads.clear();
          setTimeout(() => { scanLock = false; }, 800);
        });
      });

      Quagga.onProcessed(function(result) {
        const drawingCtx = Quagga.canvas && Quagga.canvas.ctx && Quagga.canvas.ctx.overlay;
        const drawingCanvas = Quagga.canvas && Quagga.canvas.dom && Quagga.canvas.dom.overlay;
        if (!drawingCtx || !drawingCanvas) return;

        drawingCtx.clearRect(0, 0, parseInt(drawingCanvas.width), parseInt(drawingCanvas.height));

        if (result && result.boxes) {
          result.boxes
            .filter(box => box !== result.box)
            .forEach(box => {
              Quagga.ImageDebug.drawPath(
                box,
                { x: 0, y: 1 },
                drawingCtx,
                { color: "green", lineWidth: 2 }
              );
            });
        }
      });
    }

    Quagga.start();
    scannerIsActive = true;

    if (messageArea) {
      messageArea.textContent = 'Cámara iniciada. Escaneando...';
      messageArea.style.color = 'green';
    }
  });
}


// Exponer funciones a los botones inline del HTML
window.buscarProductoManual = buscarProductoManual;
window.confirmarVenta       = confirmarVenta;
window.soloConsulta         = soloConsulta;
window.startScanner         = startScanner;
