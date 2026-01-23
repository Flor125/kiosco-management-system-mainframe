    document.addEventListener('DOMContentLoaded', function() {
        const scannerBtn = document.querySelector('.scanner-btn'); 
        if (scannerBtn) {
            scannerBtn.addEventListener('click', function() {
                const url = this.getAttribute('data-url'); 
                window.location.href = url;
            });
        }
    });

    const tableContainer = document.querySelector('.table-producto');
    const listHeader = tableContainer ? tableContainer.querySelector('.list-header') : null;
    const searchBar = tableContainer ? tableContainer.querySelector('.search-bar') : null;
    const toggleSearchBtn = document.getElementById('toggleSearch');
    const searchInput = document.getElementById('productSearch');
    const rows = document.querySelectorAll('.table-producto tbody tr');

    function updateTableStickyOffsets() {
        if (!tableContainer || !listHeader || !searchBar) return;
        const headerH = listHeader.offsetHeight;
        const searchH = searchBar.classList.contains('is-open') ? searchBar.offsetHeight : 0;

        tableContainer.style.setProperty('--headerH', headerH + 'px');
        tableContainer.style.setProperty('--searchH', searchH + 'px');
    }

    if (toggleSearchBtn && searchBar && searchInput && rows.length) {
        toggleSearchBtn.addEventListener('click', () => {
            const isOpen = searchBar.classList.toggle('is-open');
            toggleSearchBtn.classList.toggle('is-active', isOpen);

            setTimeout(() => {
                if (isOpen) searchInput.focus();
                updateTableStickyOffsets();
            }, 260);
        });

        searchInput.addEventListener('input', function() {
            const filtro = this.value.trim().toLowerCase();

            // en el input de búsqueda, dentro de rows.forEach(row => { ... })
rows.forEach(row => {
    if (!row.classList.contains('alert-row')) return;

    const textoFila = row.textContent.toLowerCase();
    const id = row.dataset.alertaId;
    const ofertaRow = document.querySelector('.oferta-expand[data-parent-id="' + id + '"]');

    if (!filtro || textoFila.includes(filtro)) {
        row.style.display = '';
        if (ofertaRow) ofertaRow.style.display = 'none'; // se muestra solo al click
    } else {
        row.style.display = 'none';
        if (ofertaRow) ofertaRow.style.display = 'none';
    }
});

        });
    }

    

    updateTableStickyOffsets();
    window.addEventListener('resize', updateTableStickyOffsets);

    