// Preguntamos: "¿La pantalla es ANGOSTA (como un celular)?"
// (Usamos 767px, justo 1px menos que el CSS)
if (window.matchMedia("(max-width: 767px)").matches) {

    // --- Si SÍ es un celular, ejecutamos toda tu lógica ---
    
    const effect = document.querySelector('.effect');
    const buttons = document.querySelectorAll('.navbar button:not(.plus)');

    buttons.forEach(button => {
        button.addEventListener('click', e => {
            const x = e.target.offsetLeft;

            buttons.forEach(btn => {
                btn.classList.remove('active');
            })
            e.target.classList.add('active');

            anime({
                targets: '.effect',
                left: `${x}px`,
                opacity: '1',
                duration: 600
            });
        });
    });

    // Tu código del botón "plus" también va acá adentro
    const plusButton = document.querySelector('.navbar .plus');
    plusButton.addEventListener('click', () => {
        buttons.forEach(btn => {
            btn.classList.remove('active');
        });
        anime({
            targets: '.effect',
            opacity: 0,
            duration: 300
        });
    });

    
// Asegúrate de que este script esté en tu main.js o en un bloque <script>
document.addEventListener('DOMContentLoaded', function() {
    const navegables = document.querySelectorAll('.js-navegacion');
    
    navegables.forEach(elemento => {
        elemento.style.cursor = 'pointer'; 
        elemento.addEventListener('click', function() {
            const url = this.getAttribute('data-url');
            if (url) {
                window.location.href = url;
            }
        });
    });
});

document.addEventListener('DOMContentLoaded', () => {
        const navButtons = document.querySelectorAll('.navbar button[data-url]');

        navButtons.forEach((btn) => {
          btn.addEventListener('click', () => {
            const url = btn.dataset.url;
            if (url) {
              window.location.href = url;
            }
          });
        });

        document.querySelectorAll('.alerta-slide').forEach((slide) => {
          slide.addEventListener('click', function () {
            const url = this.dataset.url;
            if (!url) return;

            // feedback visual
            this.classList.add('is-pressed');

            // pequeño delay para que se vea el efecto
            setTimeout(() => {
              window.location.href = url;
            }, 150);
          });
        });
      });

// --- Desplegable de oferta inline ---
function setupOfertasInline(){
    const ofertaButtons = document.querySelectorAll('.btn-oferta');

    ofertaButtons.forEach(btn => {
        btn.addEventListener('click', async () => {
            const row = btn.closest('.alert-row');
            if (!row) return;

            const alertaId = row.dataset.alertaId;
            const panelRow = document.querySelector(
                '.oferta-expand[data-parent-id="' + alertaId + '"]'
            );
            if (!panelRow) return;

            const isOpen = panelRow.style.display !== 'none';

            // cerrar si ya está abierto
            if (isOpen){
                panelRow.style.display = 'none';
                return;
            }

            // abrir
            panelRow.style.display = 'table-row';

            const panel = panelRow.querySelector('.oferta-panel');
            if (!panel) return;

            // cargar candidatos solo la primera vez
            if (!panel.dataset.loaded) {
                await cargarCandidatosParaPanel(panel, alertaId);
                panel.dataset.loaded = '1';
            }

            inicializarCalculoPanel(panel);
        });
    });
}

async function cargarCandidatosParaPanel(panel, alertaId){
    const tbody = panel.querySelector('.combo-body');
    if (!tbody) return;

    tbody.innerHTML = '<tr><td colspan="5">Cargando opciones...</td></tr>';

    try{
        const resp = await fetch('/api/oferta_candidatos/' + alertaId);
        if (!resp.ok){
            tbody.innerHTML = '<tr><td colspan="5">Error al cargar sugerencias.</td></tr>';
            return;
        }
        const data = await resp.json();
        if (data.status !== 'ok' || !data.candidatos || data.candidatos.length === 0){
            tbody.innerHTML = '<tr><td colspan="5">No hay otros productos en la misma categoría con stock.</td></tr>';
            return;
        }

        tbody.innerHTML = '';
data.candidatos.forEach(c => {
    const tr = document.createElement('tr');

    const precio = Number(c.precio_venta || 0);
    const stock  = c.stock_total || 0;
    const prox   = c.proximo_venc || '—';

    tr.innerHTML = `
        <td>
            <input type="checkbox"
                   class="combo-item"
                   data-precio="${precio.toFixed(2)}"
                   data-id-producto="${c.id_producto}">
        </td>
        <td>${c.nombre}</td>
        <td>$ ${precio.toFixed(2)}</td>
        <td>${stock}</td>
        <td>${prox}</td>
    `;

    tbody.appendChild(tr);
});


    } catch(e){
        console.error(e);
        tbody.innerHTML = '<tr><td colspan="5">Error inesperado al cargar sugerencias.</td></tr>';
    }
}

async function cargarResumenAlertas(){
    try{
        const resp = await fetch('/api/alertas/resumen');
        if (!resp.ok) return;
        const data = await resp.json();
        if (data.status !== 'ok') return;

        const badge = document.getElementById('badgeAlertas');
        if (!badge) return;

        if (data.total > 0){
            badge.textContent = data.total;
            badge.style.display = 'inline-flex';
        } else {
            badge.style.display = 'none';
        }
    } catch(e){
        console.error('Error al cargar resumen de alertas', e);
    }
}

async function cargarListaAlertas(){
    const body = document.getElementById('notifBody');
    if (!body) return;

    body.innerHTML = '<div class="notif-empty">Cargando alertas...</div>';

    try{
        const resp = await fetch('/api/alertas/lista');
        if (!resp.ok){
            body.innerHTML = '<div class="notif-empty">Error al cargar.</div>';
            return;
        }
        const data = await resp.json();
        if (data.status !== 'ok'){
            body.innerHTML = '<div class="notif-empty">Error al cargar.</div>';
            return;
        }

        const alertas = data.alertas || [];
        if (alertas.length === 0){
            body.innerHTML = '<div class="notif-empty">Sin alertas pendientes.</div>';
            return;
        }

        body.innerHTML = '';
        alertas.forEach(a => {
            const dias = a.dias_restantes;
            let clase = '';
            let textoDias = '';

            if (dias == null){
                textoDias = 'Sin fecha';
            } else if (dias < 0){
                clase = 'vencido';
                textoDias = `Vencido hace ${Math.abs(dias)} día(s)`;
            } else if (dias === 0){
                clase = 'hoy';
                textoDias = 'Vence hoy';
            } else {
                clase = 'proximo';
                textoDias = `Vence en ${dias} día(s)`;
            }

            const div = document.createElement('div');
            div.className = 'notif-item ' + clase;
            div.innerHTML = `
                <div class="title">${a.nombre || 'Producto'}</div>
                <div class="meta">
                    ${a.categoria || 'Sin categoría'} • ${textoDias}
                </div>
            `;
            body.appendChild(div);
        });

    } catch(e){
        console.error('Error al cargar lista de alertas', e);
        body.innerHTML = '<div class="notif-empty">Error inesperado.</div>';
    }
}

function setupNotificacionesPanel(){
    const btn = document.getElementById('btnNotificaciones');
    const panel = document.getElementById('notifPanel');
    if (!btn || !panel) return;

    let abierto = false;

    btn.addEventListener('click', async (e) => {
        e.stopPropagation();
        abierto = !abierto;
        if (abierto){
            panel.style.display = 'flex';
            await cargarListaAlertas();
        } else {
            panel.style.display = 'none';
        }
    });

    // cerrar al tocar fuera
    document.addEventListener('click', (e) => {
        if (!abierto) return;
        if (!panel.contains(e.target) && e.target !== btn){
            panel.style.display = 'none';
            abierto = false;
        }
    });
}
setupNotificacionesPanel();

document.addEventListener('DOMContentLoaded', () => {
    cargarResumenAlertas();
    // si querés, recargar cada X minutos:
    // setInterval(cargarResumenAlertas, 5 * 60 * 1000);
});


function inicializarCalculoPanel(panel){
    // Precio del producto en alerta (el que siempre entra en el combo)
    let precioProductoAlerta = Number(panel.dataset.precioBase);
    if (Number.isNaN(precioProductoAlerta)) {
        precioProductoAlerta = 0;
    }

    const baseEl   = panel.querySelector('.precio-base');
    const sugEl    = panel.querySelector('.precio-sugerido');
    const inputGan = panel.querySelector('.ganancia-input');
    const checkboxes = panel.querySelectorAll('.combo-item');

    function recalcular(){
        // 1) siempre arrancamos con el producto en alerta
        let suma = precioProductoAlerta;

        // 2) sumar cada producto tildado
        checkboxes.forEach(cb => {
            if (cb.checked){
                let p = Number(cb.dataset.precio);
                if (Number.isNaN(p)) p = 0;
                suma += p;
            }
        });

        // 3) aplicar ganancia
        let g = Number(inputGan.value);
        if (Number.isNaN(g)) g = 0;

        const precioSugerido = suma * (1 + g / 100);

        baseEl.textContent = '$' + suma.toFixed(2);
        sugEl.textContent  = '$' + precioSugerido.toFixed(2);
    }

    inputGan.addEventListener('input', recalcular);
    checkboxes.forEach(cb => cb.addEventListener('change', recalcular));

    recalcular();
}


function setupGuardarOferta(){
    const botones = document.querySelectorAll('.guardar-oferta-btn');

    botones.forEach(btn => {
        btn.addEventListener('click', async () => {
            const panel = btn.closest('.oferta-panel');
            if (!panel) return;

            const alertaId = panel.dataset.alertaId;
            const parentRow = document.querySelector(
                '.alert-row[data-alerta-id="' + alertaId + '"]'
            );
            if (!parentRow) return;

            // --- 1) datos base ---
            const idBase = parseInt(parentRow.dataset.idProducto, 10);
            const nombreBase = parentRow.querySelector('strong')
                                 ? parentRow.querySelector('strong').textContent.trim()
                                 : 'Oferta alerta ' + alertaId;

            const ganancia = parseFloat(
                panel.querySelector('.ganancia-input').value || '0'
            );

            const textoPrecio = panel.querySelector('.precio-sugerido')
                                     .textContent
                                     .replace(/[^\d.,]/g, '')
                                     .replace(',', '.');
            const precioCombo = parseFloat(textoPrecio) || 0;

            // --- 2) productos seleccionados + nombres extras ---
            const productos = [];
            const nombresExtras = [];

            // producto en alerta siempre entra si el id es válido
            if (!Number.isNaN(idBase)) {
                productos.push({ id_producto: idBase, cantidad: 1 });
            }

            panel.querySelectorAll('.combo-item').forEach(cb => {
                if (cb.checked){
                    const idProd = parseInt(cb.dataset.idProducto, 10);
                    if (!Number.isNaN(idProd)){
                        productos.push({ id_producto: idProd, cantidad: 1 });

                        // sacamos el nombre de la segunda columna de esa fila
                        const row = cb.closest('tr');
                        if (row && row.children[1]) {
                            const nombreProd = row.children[1].textContent.trim();
                            if (nombreProd) {
                                nombresExtras.push(nombreProd);
                            }
                        }
                    }
                }
            });

            if (productos.length === 0){
                alert('No hay productos en la oferta.');
                return;
            }

            // --- 3) construir un nombre más descriptivo de la oferta ---
            let nombreOferta = 'Oferta ' + nombreBase;
            if (nombresExtras.length === 1) {
                nombreOferta += ' + ' + nombresExtras[0];
            } else if (nombresExtras.length > 1) {
                // Si querés todo:
                // nombreOferta += ' + ' + nombresExtras.join(' + ');
                // Si querés más corto:
                nombreOferta += ' + combo (' + nombresExtras.join(' + ') + ')';
            }

            const payload = {
                nombre: nombreOferta,
                ganancia: ganancia,
                precio_combo: precioCombo,
                productos: productos
            };

            try{
                const resp = await fetch('/ofertas/guardar', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify(payload)
                });
                const data = await resp.json();

                if (resp.ok && data.status === 'ok'){
                    alert('Oferta guardada (ID ' + data.id_oferta + ').');
                    // acá podrías marcar la alerta como atendida u ocultarla
                } else {
                    alert('Error al guardar la oferta.');
                    console.log(data);
                }
            } catch(e){
                console.error(e);
                alert('Error de conexión al guardar la oferta.');
            }
        });
    });
}


// Llamalo después de setupOfertasInline():
setupOfertasInline();
setupGuardarOferta();



    // --- Fin del código de celular ---

} else {
    // (Acá podrías poner la lógica para el menú de DESKTOP,
    //  si es que tiene animaciones)
}

const effect = document.querySelector('.effect');
const buttons = document.querySelectorAll('.navbar button:not(.plus)');

buttons.forEach(button => {
    button.addEventListener('click', e => {
        const x = e.target.offsetLeft;

        buttons.forEach(btn => {
            btn.classList.remove('active');
        })
        e.target.classList.add('active');

        anime({
            targets: '.effect',
            left: `${x}px`,
            opacity: '1',
            duration: 600
        })
    })
})

// 1. Seleccionamos el botón 'plus' por separado
const plusButton = document.querySelector('.navbar .plus');

// 2. Le agregamos su propio Event Listener
plusButton.addEventListener('click', () => {
    // 3. (Importante) Borramos el 'active' de todos los OTROS botones
    buttons.forEach(btn => {
        btn.classList.remove('active');
    });
    // 4. Usamos 'anime' para animar la desaparición del 'effect'
    anime({
        targets: '.effect',
        opacity: 0,
        duration: 300 // (Un poco más rápido para que desaparezca)
    });
});

var swiper = new Swiper('.carousel', {
    navigation: {
        nextEl: '.swiper-button-next',
        prevEl: '.swiper-button-prev',
    },
    pagination: {
        el: '.swiper-pagination',
        clickable: true,
    },
    autoplay: {
        delay: 5000,
        disableOnInteraction: false,
    },
    loop: true,
})



