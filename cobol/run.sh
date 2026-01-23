#!/bin/bash
set -e

cd "$(dirname "$0")"

EXEC="./batchcosto"
LOGFILE="run.log"

if [[ ! -x "$EXEC" ]]; then
  echo "[$(date +'%F %T')] [ERROR] Ejecutable $EXEC no encontrado o no es ejecutable." | tee -a "$LOGFILE"
  exit 1
fi

echo "[$(date +'%F %T')] [INFO] Ejecutando batchcosto..." | tee -a "$LOGFILE"
"$EXEC" >> "$LOGFILE" 2>&1
RET_CODE=$?

if [[ $RET_CODE -eq 0 ]]; then
  echo "[$(date +'%F %T')] [OK] Ejecución finalizada correctamente (RETURN CODE: $RET_CODE)." | tee -a "$LOGFILE"
else
  echo "[$(date +'%F %T')] [FAIL] El batch terminó con errores (RETURN CODE: $RET_CODE)." | tee -a "$LOGFILE"
fi

exit $RET_CODE
