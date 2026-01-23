import ibm_db
from pathlib import Path

DB_CONN_STR = "DATABASE=ventas;HOSTNAME=localhost;PORT=50000;PROTOCOL=TCPIP;UID=db2inst1;PWD=pass123;"

DATA_DIR = Path(__file__).resolve().parent / "data"
DATA_DIR.mkdir(exist_ok=True)
OUT_FILE = DATA_DIR / "VENTAS_IN.DAT"

SQL = """
SELECT
    ID_VENTA,
    FE
"""