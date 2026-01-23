       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALC-COSTOS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VENTAS-IN ASSIGN TO "../data/VENTAS_IN.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT COSTOS-OUT ASSIGN TO "../data/COSTOS_OUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  VENTAS-IN.
       01  VENTAS-REG.
           05 V-CODIGO        PIC X(10).
           05 V-LOTE          PIC X(8).
           05 V-FEC-VENC      PIC X(8).
           05 V-COSTO-CENT    PIC 9(9).
           05 V-PRECIO-CENT   PIC 9(9).
           05 V-CANTIDAD      PIC 9(9).

       FD  COSTOS-OUT.
       01  COSTOS-REG.
           05 C-CODIGO        PIC X(10).
           05 C-LOTE          PIC X(8).
           05 C-FEC-VENC      PIC X(8).
           05 C-COSTO-UNIT    PIC 9(7)V99.
           05 C-PRECIO-UNIT   PIC 9(7)V99.
           05 C-CANTIDAD      PIC 9(9).
           05 C-COSTO-TOT     PIC 9(11)V99.
           05 C-MARGEN-PCT    PIC 9(3)V9(2).

       WORKING-STORAGE SECTION.
       01 WS-EOF              PIC X VALUE "N".
       01 WS-COSTO            PIC 9(7)V99.
       01 WS-PRECIO           PIC 9(7)V99.
       01 WS-CANTIDAD         PIC 9(9).
       01 WS-COSTO-TOT        PIC 9(11)V99.
       01 WS-MARGEN-PCT       PIC 9(3)V9(2).

       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT  VENTAS-IN
                OUTPUT COSTOS-OUT

           PERFORM LEER-VENTA

           PERFORM UNTIL WS-EOF = "S"
              PERFORM PROCESAR-VENTA
              PERFORM LEER-VENTA
           END-PERFORM

           CLOSE VENTAS-IN COSTOS-OUT
           STOP RUN.

       LEER-VENTA.
           READ VENTAS-IN
              AT END
                 MOVE "S" TO WS-EOF
           END-READ.

       PROCESAR-VENTA.
           *> Los centavos vienen como entero (ej: 00010050 = 100,50)
           MOVE V-COSTO-CENT  TO WS-COSTO
           MOVE V-PRECIO-CENT TO WS-PRECIO
           MOVE V-CANTIDAD    TO WS-CANTIDAD

           COMPUTE WS-COSTO-TOT = WS-COSTO * WS-CANTIDAD

           IF WS-PRECIO = 0
              MOVE 0 TO WS-MARGEN-PCT
           ELSE
              COMPUTE WS-MARGEN-PCT =
                  (WS-PRECIO - WS-COSTO)
                  / WS-PRECIO * 100
           END-IF

           MOVE V-CODIGO      TO C-CODIGO
           MOVE V-LOTE        TO C-LOTE
           MOVE V-FEC-VENC    TO C-FEC-VENC
           MOVE WS-COSTO      TO C-COSTO-UNIT
           MOVE WS-PRECIO     TO C-PRECIO-UNIT
           MOVE WS-CANTIDAD   TO C-CANTIDAD
           MOVE WS-COSTO-TOT  TO C-COSTO-TOT
           MOVE WS-MARGEN-PCT TO C-MARGEN-PCT

           WRITE COSTOS-REG.
