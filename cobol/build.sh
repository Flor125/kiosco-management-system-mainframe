#!/bin/bash
set -e

# Siempre compilar relativo a esta carpeta
cd "$(dirname "$0")"

echo "Compilando batchcosto (GnuCOBOL)..."

# Compilar y linkear en un solo paso
cobc -x -o batchcosto batchcosto.cob

echo "✔ Binario 'batchcosto' creado correctamente."
chmod +x batchcosto
