       IDENTIFICATION DIVISION.
       PROGRAM-ID.  BATCHCOSTOS.
       AUTHOR.      FLORENCIA SOMBRA.
       INSTALLATION. COBOL DEVELOPMENT CENTER.
       DATE-WRITTEN. 2025-11-09.
       SECURITY.    NON-CONFIDENTIAL.

      ******************************************************************
      * OBJETIVO: Procesar archivos de entrada para calcular costos
      * promedio y generar alertas de vencimiento.
      *
      * ESTRATEGIA: CERO SQL. Este programa es un cerebro de lógica
      * pura. Lee archivos, procesa, escribe archivos.
      *
      * ENTRADA:
      * - costos.dat:       Archivo con datos de Lote + Producto
      * - vencimientos.dat: Archivo con IDs de lotes a vencer
      *
      * SALIDA:
      * - historico.dat:    Archivo con el nuevo costo promedio
      * - alertas.dat:      Archivo con los IDs de lote para alertar
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z.
       OBJECT-COMPUTER. IBM-Z.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Archivo de entrada de lotes (JOIN de LOTE + PRODUCTO)
           SELECT COSTOS-IN-FILE ASSIGN TO "costos.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-COSTOS.

      * Archivo de entrada de vencimientos (Lotes a vencer)
           SELECT VENCIMIENTOS-IN-FILE ASSIGN TO "vencimientos.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-VENCIMIENTOS.

      * Archivo de salida para el historico de precios
           SELECT HISTORICO-OUT-FILE ASSIGN TO "historico.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-HISTORICO.

      * Archivo de salida para las alertas
           SELECT ALERTAS-OUT-FILE ASSIGN TO "alertas.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-ALERTAS.

       DATA DIVISION.
       FILE SECTION.
      *-----------------------------------------------------------------
      * Def. del archivo de entrada para costos (Formato TEXTO)
      * Python escribe: ID(9) | GANANCIA(6,"25.50") | CANT(9) | COSTO(11,"150.75")
      *-----------------------------------------------------------------
       FD  COSTOS-IN-FILE.
       01  COSTOS-IN-RECORD.
           05 CR-PRODUCTO-ID       PIC X(9).
           05 CR-PORC-GANANCIA     PIC X(6).
           05 CR-CANTIDAD          PIC X(9).
           05 CR-PRECIOCOSTO       PIC X(11).

      *-----------------------------------------------------------------
      * Def. del archivo de entrada para vencimientos (Formato TEXTO)
      *-----------------------------------------------------------------
       FD  VENCIMIENTOS-IN-FILE.
       01  VENCIMIENTOS-IN-RECORD.
           05 VR-LOTE-ID           PIC X(9).

      *-----------------------------------------------------------------
      * Def. del archivo de salida para historico (Formato TEXTO)
      * COBOL escribe: ID(9) | COSTOPROM(11,"150.75") | GANANCIA(6,"25.50")
      *-----------------------------------------------------------------
       FD  HISTORICO-OUT-FILE.
       01  HISTORICO-OUT-RECORD.
           05 HR-PRODUCTO-ID       PIC X(9).
           05 HR-COSTO-PROMEDIO    PIC X(11).
           05 HR-PORC-GANANCIA     PIC X(6).

      *-----------------------------------------------------------------
      * Def. del archivo de salida para alertas (Formato TEXTO)
      *-----------------------------------------------------------------
       FD  ALERTAS-OUT-FILE.
       01  ALERTAS-OUT-RECORD.
           05 AR-LOTE-ID           PIC X(9).

       WORKING-STORAGE SECTION.

      *-----------------------------------------------------------------
      * Variables de Lógica de Negocio (El "Cerebro")
      *-----------------------------------------------------------------
       01  WS-CALCULOS.
           05 WS-TOTAL-COSTO-VALOR    PIC S9(13)V99 COMP-3 VALUE +0.
           05 WS-TOTAL-CANTIDAD       PIC S9(9)     COMP   VALUE +0.
           05 WS-COSTO-PROMEDIO       PIC S9(8)V99  COMP-3 VALUE +0.
           05 WS-PORC-GANANCIA        PIC S9(3)V99  COMP-3 VALUE +0.

      *-----------------------------------------------------------------
      * Variables de Trabajo (Conversión de texto a número)
      *-----------------------------------------------------------------
       01  WS-CURRENT-LOTE-DATA.
           05 WS-PRODUCTO-ID-N        PIC 9(9).
           05 WS-PORC-GANANCIA-N      PIC S9(3)V99.
           05 WS-CANTIDAD-N           PIC S9(9).
           05 WS-PRECIOCOSTO-N        PIC S9(8)V99.

       01  WS-HISTORICO-OUT-DATA.
           05 WS-HR-COSTO-PROMEDIO    PIC 9(8)V99.
           05 WS-HR-PORC-GANANCIA     PIC 9(3)V99.
           05 WS-HR-PRODUCTO-ID       PIC 9(9).

      *-----------------------------------------------------------------
      * Variables de Control
      *-----------------------------------------------------------------
       01  WS-CONTROL.
           05 WS-CONTADOR-ALERTAS     PIC 9(4) VALUE 0.
           05 WS-CONTADOR-COSTOS      PIC 9(4) VALUE 0.

       01  WS-FILE-STATUS-VALUES.
           05 FS-COSTOS               PIC X(2).
           05 FS-VENCIMIENTOS         PIC X(2).
           05 FS-HISTORICO            PIC X(2).
           05 FS-ALERTAS              PIC X(2).
           05 FS-ERROR-MSG            PIC X(2).
               88 FS-OK               VALUE "00".
               88 FS-EOF              VALUE "10".

       01  WS-EOF-SWITCHES.
           05 EOF-COSTOS              PIC X VALUE 'N'.
              88 NO-HAY-MAS-COSTOS         VALUE 'Y'.
           05 EOF-VENCIMIENTOS        PIC X VALUE 'N'.
              88 NO-HAY-MAS-VENCIMIENTOS   VALUE 'Y'.

      *-----------------------------------------------------------------
      * Lógica de Corte de Control
      *-----------------------------------------------------------------
       01  WS-CONTROL-BREAK.
           05 WS-PREV-PRODUCTO-ID     PIC 9(9) VALUE 0.

       77  WS-RETURN-CODE             PIC S9(4) COMP VALUE 0.

       PROCEDURE DIVISION.
       0000-MAIN-PROCEDURE.
           DISPLAY '--- INICIO BATCH COBOL (BATCHCOSTOS) ---'.
           PERFORM 2000-PROCESAR-COSTOS.
           PERFORM 3000-PROCESAR-ALERTAS.
           DISPLAY '--- FIN BATCH COBOL ---'.
           DISPLAY 'COSTOS PROMEDIO ACTUALIZADOS: ' WS-CONTADOR-COSTOS.
           DISPLAY 'ALERTAS DE VENCIMIENTO: '       WS-CONTADOR-ALERTAS.
           GOBACK.

      ******************************************************************
      * Párrafo 2000: Lógica de Corte de Control para Costos
      ******************************************************************
       2000-PROCESAR-COSTOS.
           DISPLAY '--- INICIO PROCESAMIENTO DE COSTOS ---'.

           OPEN INPUT  COSTOS-IN-FILE
                OUTPUT HISTORICO-OUT-FILE.

      * Lectura inicial (Priming Read)
           PERFORM 2010-LEER-COSTOS

           IF NOT NO-HAY-MAS-COSTOS
               MOVE WS-PRODUCTO-ID-N   TO WS-PREV-PRODUCTO-ID
               MOVE WS-PORC-GANANCIA-N TO WS-PORC-GANANCIA
           END-IF

           PERFORM UNTIL NO-HAY-MAS-COSTOS

               IF WS-PRODUCTO-ID-N NOT = WS-PREV-PRODUCTO-ID
      * Procesamos el producto anterior
                   PERFORM 2150-CALCULAR-Y-GRABAR
               END-IF
      * Acumulamos el lote actual
               PERFORM 2100-ACUMULAR-TOTALES
               PERFORM 2010-LEER-COSTOS
           END-PERFORM

      * Procesamos el último grupo de productos (cuando AT END)
           IF WS-PREV-PRODUCTO-ID NOT = 0
               PERFORM 2150-CALCULAR-Y-GRABAR
           END-IF

           CLOSE COSTOS-IN-FILE
                 HISTORICO-OUT-FILE.
                 

       2010-LEER-COSTOS.
           READ COSTOS-IN-FILE
               AT END
                   SET NO-HAY-MAS-COSTOS TO TRUE
               NOT AT END
                   PERFORM 2020-CONVERTIR-DATOS-COSTOS
           END-READ.

       2020-CONVERTIR-DATOS-COSTOS.
      * Convierte los datos de TEXTO (PIC X) a NUMERICO
           MOVE FUNCTION NUMVAL(CR-PRODUCTO-ID) TO WS-PRODUCTO-ID-N
           MOVE FUNCTION NUMVAL(CR-CANTIDAD)    TO WS-CANTIDAD-N
           
           MOVE FUNCTION NUMVAL(CR-PRECIOCOSTO) TO WS-PRECIOCOSTO-N
           MOVE FUNCTION NUMVAL(CR-PORC-GANANCIA) TO WS-PORC-GANANCIA-N
           .

       2100-ACUMULAR-TOTALES.
      * Esta es tu lógica de negocio
           COMPUTE WS-TOTAL-COSTO-VALOR =
                   WS-TOTAL-COSTO-VALOR +
                   (WS-PRECIOCOSTO-N * WS-CANTIDAD-N)
           ADD WS-CANTIDAD-N TO WS-TOTAL-CANTIDAD.

       2150-CALCULAR-Y-GRABAR.
      * Se ejecuta en el "corte" (cuando cambia el ID de producto)
           IF WS-TOTAL-CANTIDAD > 0
               COMPUTE WS-COSTO-PROMEDIO ROUNDED =
                       WS-TOTAL-COSTO-VALOR / WS-TOTAL-CANTIDAD
               
               MOVE WS-COSTO-PROMEDIO TO WS-HR-COSTO-PROMEDIO
               MOVE WS-PORC-GANANCIA  TO WS-HR-PORC-GANANCIA
               MOVE WS-PREV-PRODUCTO-ID TO WS-HR-PRODUCTO-ID
               
               PERFORM 2200-GUARDAR-HISTORICO
           END-IF.

      * Reseteamos para el próximo producto
           MOVE +0 TO WS-TOTAL-COSTO-VALOR
                      WS-TOTAL-CANTIDAD
                      WS-COSTO-PROMEDIO.
      * Guardamos el ID y % del *nuevo* producto
           MOVE WS-PRODUCTO-ID-N TO WS-PREV-PRODUCTO-ID.
           MOVE WS-PORC-GANANCIA-N TO WS-PORC-GANANCIA.

       2200-GUARDAR-HISTORICO.
      * Escribe el resultado en el archivo de salida
           MOVE WS-HR-PRODUCTO-ID TO HR-PRODUCTO-ID
           
           MOVE WS-HR-COSTO-PROMEDIO TO WS-PRECIOCOSTO-N
           STRING WS-PRECIOCOSTO-N DELIMITED BY SIZE
             INTO HR-COSTO-PROMEDIO
           END-STRING
           
           MOVE WS-HR-PORC-GANANCIA TO WS-PORC-GANANCIA-N
           STRING WS-PORC-GANANCIA-N DELIMITED BY SIZE
             INTO HR-PORC-GANANCIA
           END-STRING

           WRITE HISTORICO-OUT-RECORD
           IF FS-HISTORICO NOT = "00"
               DISPLAY "ERROR ESCRIBIENDO historico.dat: " FS-HISTORICO
               PERFORM 9900-ERROR-FATAL
           ELSE
               ADD 1 TO WS-CONTADOR-COSTOS
           END-IF.

      ******************************************************************
      * Párrafo 3000: Lógica de Vencimientos
      ******************************************************************
       3000-PROCESAR-ALERTAS.
      * Lee el archivo de vencimientos y escribe alertas
           DISPLAY '--- INICIO PROCESAMIENTO DE ALERTAS POR VENCER ---'.

           OPEN INPUT  VENCIMIENTOS-IN-FILE
                OUTPUT ALERTAS-OUT-FILE

        PERFORM UNTIL NO-HAY-MAS-VENCIMIENTOS
               READ VENCIMIENTOS-IN-FILE
                   AT END
                       SET NO-HAY-MAS-VENCIMIENTOS TO TRUE
                   NOT AT END
                       PERFORM 3100-INSERTAR-ALERTA
               END-READ
           END-PERFORM

           CLOSE VENCIMIENTOS-IN-FILE
                 ALERTAS-OUT-FILE.
                DISPLAY '3000: PROCESAMIENTO DE ALERTAS FINALIZADO.'.

       3100-INSERTAR-ALERTA.
      * Mueve el ID leído (texto) al archivo de salida (texto)
           MOVE VR-LOTE-ID TO AR-LOTE-ID
           WRITE ALERTAS-OUT-RECORD
           IF FS-ALERTAS NOT = "00"
               DISPLAY "ERROR ESCRIBIENDO alertas.dat: " FS-ALERTAS
               PERFORM 9900-ERROR-FATAL
           ELSE
               ADD 1 TO WS-CONTADOR-ALERTAS
           END-IF.

      ******************************************************************
      * Párrafos de Error y Utilitarios
      ******************************************************************
       9900-ERROR-FATAL.
           DISPLAY '!!! ERROR CATASTROFICO EN BATCH !!!'.
           
           CLOSE COSTOS-IN-FILE
                 HISTORICO-OUT-FILE
                 VENCIMIENTOS-IN-FILE
                 ALERTAS-OUT-FILE.

           MOVE 8 TO WS-RETURN-CODE
           MOVE WS-RETURN-CODE TO RETURN-CODE
           GOBACK.
