      *----------------------------------------------------------------*
      * Purpose: Merge de dos archivos
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.

       PROGRAM-ID. CL12EJ01.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *----Archivo de entrada A
       SELECT VENTASA
           ASSIGN TO '../VENTASA.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-VENTASA.

      *----Archivo de entrada B
       SELECT VENTASB
           ASSIGN TO '../VENTASB.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-VENTASB.

      *----Archivo de Salida con todos los registros VENTATOT.TXT
      *----Archivo de salida C
       SELECT VENTATOT
           ASSIGN TO '../VENTATOT.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-VENTATOT.

      *----Archivo de Salida con registros de control TOTALES.TEXT
      *----Archivo de salida D
       SELECT TOTALES
           ASSIGN TO '../TOTALES.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-TOTALES.

      *----------------------------------------------------------------*

       DATA DIVISION.

       FILE SECTION.
      *----Definicion registro archivo A
       FD VENTASA.
       01 ENTRADA-A.
          05 ENTRADA-A-FECHA               PIC X(10).
          05 ENTRADA-A-VENDEDOR            PIC X(30).
          05 ENTRADA-A-IMPORTE             PIC 9(8)V9(2).
      *----Definicion registro archivo B
       FD VENTASB.
       01 ENTRADA-B.
          05 ENTRADA-B-FECHA               PIC X(10).
          05 ENTRADA-B-VENDEDOR            PIC X(30).
          05 ENTRADA-B-IMPORTE             PIC 9(8)V9(2).
      *----Definicion registro archivo con todos los registros.
      *----Mismo formato que los de entrada
       FD VENTATOT.
       01 SALIDA-TOT.
          05 SALIDA-FECHA               PIC X(10).
          05 SALIDA-VENDEDOR            PIC X(30).
          05 SALIDA-IMPORTE             PIC 9(8)V9(2).
      *----Definicion registro archivo con registros de totales.
      *----Diferente formato que los de entrada ,

       FD TOTALES.
       01 REGISTROS-TOT.
          05 TOT-ARCHIVO                 PIC X(01).
          05 TOT-REGISTROS               PIC 9(04).
          05 TOT-IMPORTE                 PIC 9(8)V9(2).

      *----------------------------------------------------------------*

       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-VENTASA                      PIC X(2).
             88 FS-VENTASA-OK                VALUE '00'.
             88 FS-VENTASA-EOF               VALUE '10'.
          05 FS-VENTASB                      PIC X(2).
             88 FS-VENTASB-OK                VALUE '00'.
             88 FS-VENTASB-EOF               VALUE '10'.
          05 FS-VENTATOT                     PIC X(2).
             88 FS-VENTATOT-OK               VALUE '00'.
             88 FS-VENTATOT-EOF              VALUE '10'.
          05 FS-TOTALES                   PIC X(2).
             88 FS-TOTALES-OK               VALUE '00'.
             88 FS-TOTALES-EOF              VALUE '10'.
       01 WS-VENTASA-EOF                     PIC X(1).
          88 WS-VENTASA-EOF-YES              VALUE 'Y'.
          88 WS-VENTASA-EOF-NO               VALUE 'N'.
       01 WS-VENTASB-EOF                     PIC X(1).
          88 WS-VENTASB-EOF-YES              VALUE 'Y'.
          88 WS-VENTASB-EOF-NO               VALUE 'N'.
       01 WS-VENTATOT-EOF                     PIC X(1).
          88 WS-VENTATOT-EOF-YES              VALUE 'Y'.
          88 WS-VENTATOT-EOF-NO               VALUE 'N'.
       01 WS-TOTALES-EOF                     PIC X(1).
          88 WS-TOTALES-EOF-YES              VALUE 'Y'.
          88 WS-TOTALES-EOF-NO               VALUE 'N'.
       01 WS-VAR-AUX.
          05 WS-VENTASA-NOMBRE               PIC X(01).
          05 WS-VENTASB-NOMBRE               PIC X(01).
          05 WS-VENTASA-CANT-REG             PIC 9(2)      VALUE 0.
          05 WS-VENTASB-CANT-REG             PIC 9(2)      VALUE 0.
          05 WS-SALIDA-CANT-REG              PIC 9(5)      VALUE 0.
          05 WS-TOTALES-CANT-REG             PIC 9(5)      VALUE 0.
          05 WS-VENTASA-IMPORTE-TOT          PIC 9(8)V9(2) VALUE 0.
          05 WS-VENTASB-IMPORTE-TOT          PIC 9(8)V9(2) VALUE 0.
          05 WS-ENTRADA-IMP-TOTAL            PIC 9(8)V9(2) VALUE 0.
          05 WS-ENTRADA-IMP-FORMATO          PIC $ZZ.ZZZ.ZZ9,99.
          05 WS-TOT-AGUINALDOS-N             PIC 9(08)v99  VALUE 0.
          05 WS-SUELDO-PROM-N                PIC 9(08)v99  VALUE 0.
          05 WS-TOT-AGUINALDOS               PIC $ZZ.ZZZ.ZZ9,99.
          05 WS-SUELDO-PROM                  PIC $ZZ.ZZZ.ZZ9,99.
          05 WS-SUELDO-MAYOR-N               PIC 9(08)v99  VALUE 0.
          05 WS-SUELDO-MAYOR                 PIC $ZZ.ZZZ.ZZ9,99.
          05 WS-MENOR                        PIC X(10).

       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

           PERFORM 1000-ABRIR-ARCHIVOS
              THRU 1000-ABRIR-ARCHIVOS-EXIT.

           IF FS-VENTASA-OK OR FS-VENTASB-OK

              PERFORM 2000-PROCESAR-ARCHIVO
                 THRU 2000-PROCESAR-ARCHIVO-EXIT
                UNTIL WS-VENTASA-EOF-YES AND
                      WS-VENTASB-EOF-YES

              PERFORM 3000-FINALIZAR-PROGRAMA
                 THRU 3000-FINALIZAR-PROGRAMA-EXIT

              PERFORM 4000-TOTALES-ARCHIVO
                 THRU 4000-TOTALES-ARCHIVO-EXIT

           END-IF.

           STOP RUN.
      *----------------------------------------------------------------*
       1000-ABRIR-ARCHIVOS.

           PERFORM 1100-ABRIR-ARCHIVO-VENTASA
              THRU 1100-ABRIR-ARCHIVO-VENTASA-EXIT.

           PERFORM 1200-ABRIR-ARCHIVO-VENTASB
              THRU 1200-ABRIR-ARCHIVO-VENTASB-EXIT.

           PERFORM 1300-ABRIR-ARCHIVO-SALIDA
              THRU 1300-ABRIR-ARCHIVO-SALIDA-EXIT.

           PERFORM 1400-ABRIR-ARCHIVO-TOTALES
              THRU 1400-ABRIR-ARCHIVO-TOTALES-EXIT.

       1000-ABRIR-ARCHIVOS-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1100-ABRIR-ARCHIVO-VENTASA.

           OPEN INPUT VENTASA.

           EVALUATE FS-VENTASA
               WHEN '00'
                    PERFORM 1500-LEER-VENTASA
                       THRU 1500-LEER-VENTASA-EXIT
               WHEN '35'
                    SET WS-VENTASA-EOF-YES       TO TRUE
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE VENTASA'
                    DISPLAY 'FILE STATUS: ' FS-VENTASA
      * SI NO ABRE EL ARCHIVO DETENGO EL PROCESO
                    STOP RUN
               WHEN OTHER
                    SET WS-VENTASA-EOF-YES       TO TRUE
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE VENTASA'
                    DISPLAY 'FILE STATUS: ' FS-VENTASA
      * SI NO ABRE EL ARCHIVO DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       1100-ABRIR-ARCHIVO-VENTASA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1200-ABRIR-ARCHIVO-VENTASB.

           OPEN INPUT VENTASB.

           EVALUATE FS-VENTASB
               WHEN '00'
                    PERFORM 1600-LEER-VENTASB
                       THRU 1600-LEER-VENTASB-EXIT
               WHEN '35'
                    SET WS-VENTASB-EOF-YES       TO TRUE
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE VENTASB'
                    DISPLAY 'FILE STATUS: ' FS-VENTASB
      * SI NO EXISTE EL ARCHIVO B DETENGO EL PROCESO
                    STOP RUN
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE VENTASB'
                    DISPLAY 'FILE STATUS: ' FS-VENTASB
      * SI NO ABRE EL ARCHIVO DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       1200-ABRIR-ARCHIVO-VENTASB-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1300-ABRIR-ARCHIVO-SALIDA.

           OPEN OUTPUT VENTATOT.

           EVALUATE FS-VENTATOT
               WHEN '00'
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE VENTATOT'
                    DISPLAY 'FILE STATUS: ' FS-VENTATOT
      * SI NO ABRE EL ARCHIVO DE SALIDA DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       1300-ABRIR-ARCHIVO-SALIDA-EXIT.
           EXIT.

      *-----------------------------------------------------------------*
       1400-ABRIR-ARCHIVO-TOTALES.

           OPEN OUTPUT TOTALES.

           EVALUATE FS-TOTALES
               WHEN '00'
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE VENTATOT'
                    DISPLAY 'FILE STATUS: ' FS-TOTALES
      * SI NO ABRE EL ARCHIVO DE SALIDA DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       1400-ABRIR-ARCHIVO-TOTALES-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1500-LEER-VENTASA.

           READ VENTASA.

           EVALUATE FS-VENTASA
               WHEN '00'
                    CONTINUE
                    ADD 1                      TO WS-VENTASA-CANT-REG
               WHEN '10'
                    SET WS-VENTASA-EOF-YES     TO TRUE
                    MOVE HIGH-VALUES   TO ENTRADA-A-FECHA
               WHEN OTHER
                    SET WS-VENTASA-EOF-YES     TO TRUE
                    MOVE HIGH-VALUES   TO ENTRADA-A-FECHA
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE VENTASA'
                    DISPLAY 'FILE STATUS: ' FS-VENTASA
           END-EVALUATE.
       1500-LEER-VENTASA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1600-LEER-VENTASB.

           READ VENTASB.

           EVALUATE FS-VENTASB
               WHEN '00'
                    CONTINUE
                    ADD 1                      TO WS-VENTASB-CANT-REG
               WHEN '10'
                    SET WS-VENTASB-EOF-YES     TO TRUE
                    MOVE HIGH-VALUES   TO ENTRADA-B-FECHA
               WHEN OTHER
                    SET WS-VENTASB-EOF-YES     TO TRUE
                    MOVE HIGH-VALUES   TO ENTRADA-B-FECHA
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE VENTASB'
                    DISPLAY 'FILE STATUS: ' FS-VENTASB
           END-EVALUATE.
       1600-LEER-VENTASB-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2000-PROCESAR-ARCHIVO.

      *----SI AUN LOS DOS ARCHIVOS TIENEN REGISTROS POR LEER
           IF FS-VENTASA-OK AND FS-VENTASB-OK
      *----SELECCIONO LA FECHA MENOR
              IF ENTRADA-A-FECHA <= ENTRADA-B-FECHA
                  MOVE ENTRADA-A-FECHA TO WS-MENOR
                  PERFORM 2100-PROCESAR-VENTASA
                     THRU 2100-PROCESAR-VENTASA-EXIT
                    UNTIL NOT FS-VENTASA-OK
                       OR ENTRADA-A-FECHA > ENTRADA-B-FECHA
              ELSE
                  PERFORM 2200-PROCESAR-VENTASB
                     THRU 2200-PROCESAR-VENTASB-EXIT
                    UNTIL NOT FS-VENTASB-OK
                       OR ENTRADA-B-FECHA > ENTRADA-A-FECHA
              END-IF
           ELSE
      *----Si entra por aca,es porque uno de los dos archivos no tiene
      *----mas registros. me fijo cual es y lo proceso hasta el fin
               IF FS-VENTASA-OK
                  PERFORM 2100-PROCESAR-VENTASA
                     THRU 2100-PROCESAR-VENTASA-EXIT
                    UNTIL NOT FS-VENTASA-OK
               ELSE
                   PERFORM 2200-PROCESAR-VENTASB
                      THRU 2200-PROCESAR-VENTASB-EXIT
                     UNTIL NOT FS-VENTASB-OK
               END-IF

           END-IF.


       2000-PROCESAR-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-PROCESAR-VENTASA.

           MOVE ENTRADA-A TO SALIDA-TOT.


           PERFORM 2500-GRABAR-ARCHIVO-SAL
           THRU 2500-GRABAR-ARCHIVO-SAL-EXIT.

      *-----ACUMULO EL IMPORTE DEL ARCHIVO VENTASA

           ADD ENTRADA-A-IMPORTE TO WS-VENTASA-IMPORTE-TOT.


           PERFORM 1500-LEER-VENTASA
              THRU 1500-LEER-VENTASA-EXIT.

       2100-PROCESAR-VENTASA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2200-PROCESAR-VENTASB.

           MOVE ENTRADA-B TO SALIDA-TOT.

           PERFORM 2500-GRABAR-ARCHIVO-SAL
           THRU 2500-GRABAR-ARCHIVO-SAL-EXIT.

      *-----ACUMULO EL IMPORTE DEL ARCHIVO VENTASB

           ADD ENTRADA-B-IMPORTE TO WS-VENTASB-IMPORTE-TOT.


           PERFORM 1600-LEER-VENTASB
              THRU 1600-LEER-VENTASB-EXIT.

       2200-PROCESAR-VENTASB-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2500-GRABAR-ARCHIVO-SAL.

           WRITE SALIDA-TOT.

           EVALUATE FS-VENTATOT
               WHEN '00'
                    ADD 1 TO WS-SALIDA-CANT-REG
               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-VENTATOT
           END-EVALUATE.

       2500-GRABAR-ARCHIVO-SAL-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *----GRABO EL NUEVO ARCHIVO TOTALES

       2600-GRABAR-TOTALES.

           WRITE REGISTROS-TOT.

           EVALUATE FS-TOTALES
               WHEN '00'
                    ADD 1 TO WS-TOTALES-CANT-REG
               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-TOTALES
           END-EVALUATE.

       2600-GRABAR-TOTALES-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.

      *----MUEVO EL NOMBRE AL ARCHIVO A  Y EL CONTADOR DE REG Y EL             *
      *    ACUMULADOR DE IMPORTES "A" LOS CAMPOS DEL ARCHIVO TOTALES.

           MOVE "A" TO TOT-ARCHIVO
           MOVE WS-VENTASA-CANT-REG TO TOT-REGISTROS.
           MOVE WS-VENTASA-IMPORTE-TOT TO TOT-IMPORTE.

      *-----MUESTRO POR PANTALLA PARA CONTROL.
           DISPLAY "Archivo de control A: "
           TOT-ARCHIVO TOT-REGISTROS TOT-IMPORTE.

      *-----GRABO EL REGISTRO DE "A"

           PERFORM 2600-GRABAR-TOTALES
           THRU 2600-GRABAR-TOTALES-EXIT.

      *----MUEVO EL NOMBRE AL ARCHIVO "B"  Y EL CONTADOR DE REG Y EL             *
      *    ACUMULADOR DE IMPORTES A LOS CAMPOS DEL ARCHIVO TOTALES.

           MOVE "B" TO TOT-ARCHIVO
           MOVE WS-VENTASB-CANT-REG TO TOT-REGISTROS.
           MOVE WS-VENTASB-IMPORTE-TOT TO TOT-IMPORTE.

      *-----MUESTRO POR PANTALLA PARA CONTROL.

           DISPLAY "Archivo de control B: "
           TOT-ARCHIVO TOT-REGISTROS TOT-IMPORTE.

      *-----GRABO EL REGISTRO DE "B"

           PERFORM 2600-GRABAR-TOTALES
           THRU 2600-GRABAR-TOTALES-EXIT.

      *-----SE CIERRAN TODOS LOS ARCHIVOS.

           PERFORM 3100-CERRAR-ARCH-VENTASA
              THRU 3100-CERRAR-ARCH-VENTASA-EXIT.

           PERFORM 3200-CERRAR-ARCH-VENTASB
              THRU 3200-CERRAR-ARCH-VENTASB-EXIT.

           PERFORM 3300-CERRAR-ARCH-SAL
              THRU 3300-CERRAR-ARCH-SAL-EXIT.

           PERFORM 3400-CERRAR-ARCH-TOTAL
              THRU 3400-CERRAR-ARCH-TOTAL-EXIT.

       3000-FINALIZAR-PROGRAMA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3100-CERRAR-ARCH-VENTASA.

           CLOSE VENTASA.

           IF NOT FS-VENTASA-OK
              DISPLAY 'ERROR EN CLOSE DE VENTASA: ' FS-VENTASA
           END-IF.

       3100-CERRAR-ARCH-VENTASA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCH-VENTASB.

           CLOSE VENTASB.

           IF NOT FS-VENTASB-OK
              DISPLAY 'ERROR EN CLOSE DE VENTASB: ' FS-VENTASB
           END-IF.

       3200-CERRAR-ARCH-VENTASB-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3300-CERRAR-ARCH-SAL.

           CLOSE VENTATOT.

           IF NOT FS-VENTATOT-OK
              DISPLAY 'ERROR EN CLOSE DE SALIDA: ' FS-VENTATOT
           END-IF.

       3300-CERRAR-ARCH-SAL-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       3400-CERRAR-ARCH-TOTAL.

           CLOSE TOTALES.

           IF NOT FS-TOTALES-OK
              DISPLAY 'ERROR EN CLOSE DE SALIDA: ' FS-TOTALES
           END-IF.

       3400-CERRAR-ARCH-TOTAL-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       4000-TOTALES-ARCHIVO.

           DISPLAY '================================================'.

           DISPLAY 'CANT. REG. LEIDOS VENTAS A: ' WS-VENTASA-CANT-REG.
           DISPLAY 'CANT. REG. LEIDOS VENTAS B: ' WS-VENTASB-CANT-REG.
           DISPLAY 'CANT. REG. GRABADOS       : ' WS-SALIDA-CANT-REG.

           DISPLAY '================================================'.
       4000-TOTALES-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL12EJ01.
