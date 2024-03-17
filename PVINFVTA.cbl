       IDENTIFICATION DIVISION.
       PROGRAM-ID. PVINFVTA.
       AUTHOR. TRONCOSO LEANDRO.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICH-VENDER ASSIGN TO VENDEDOR
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS R-VENDER-LEGAJO
           FILE STATUS FS-FICH-VENDER.

           SELECT FICH-VENTA ASSIGN TO VENTAS
           ORGANIZATION IS SEQUENTIAL
           ACCESS IS SEQUENTIAL
           FILE STATUS IS FS-FICH-VENTA.

           SELECT FICH-REPORT ASSIGN TO REPORTE
           ORGANIZATION IS SEQUENTIAL
           ACCESS IS SEQUENTIAL
           FILE STATUS IS FS-FICH-REPORTE.

       DATA DIVISION.
       FILE SECTION.

       FD FICH-VENDER DATA RECORD IS R-VENDER.
       01 R-VENDER.
          05 R-VENDER-LEGAJO      PIC X(5).
          05 R-VENDER-NOMBRE      PIC X(30).
          05 R-VENDER-NACIMIENTO.
             10 R-V-N-DIA         PIC 9(2).
             10 R-V-N-MES         PIC 9(2).
             10 R-V-N-ANIO        PIC 9(4).
          05 R-VENDER-INGRESO.
             10 R-V-I-DIA         PIC 9(2).
             10 R-V-I-MES         PIC 9(2).
             10 R-V-I-ANIO        PIC 9(4).
          05 R-VENDER-CIUDAD      PIC X(26).

       FD FICH-VENTA RECORDING MODE IS F
                     DATA RECORD IS R-VENTA.
       01 R-VENTA.
          05 R-VENTA-LEGAJO       PIC X(5).
          05 R-VENTA-MONTO        PIC 9(6)V99.
          05 R-VENTA-FECHA.
             10 VENTA-DIA         PIC 99.
             10 VENTA-MES         PIC 99.
             10 VENTA-ANIO        PIC 9(4).

       FD FICH-REPORT RECORDING MODE IS F
                      DATA RECORD IS R-FICH-REPORT.
       01 R-FICH-REPORT           PIC X(80).

       WORKING-STORAGE SECTION.

       01 FS-FICH-VENDER          PIC 99.
          88 FS-FICH-VENDER-NE    VALUE 23.

       01 FS-FICH-VENTA           PIC 99.
          88 FS-FICH-VENTA-END    VALUE 10.

       01 FS-FICH-REPORTE         PIC 99.

       01 WS-GUIONES.
          05 FILLER               PIC X VALUE SPACE.
          05 FILLER               PIC X(78) VALUE ALL '-'.
          05 FILLER               PIC X VALUE SPACE.

       01 WS-TITULO.
          05 FILLER               PIC X VALUE '|'.
          05 FILLER               PIC X(30) VALUE SPACES.
          05 FILLER               PIC X(17) VALUE 'REPORTE DE VENTAS'.
          05 FILLER               PIC X(31) VALUE SPACES.
          05 FILLER               PIC X VALUE '|'.

       01 WS-SUBT-LEG.
          05 FILLER               PIC X VALUE '|'.
          05 FILLER               PIC X(4) VALUE SPACES.
          05 FILLER               PIC X(8) VALUE 'LEAGJO: '.
          05 WS-EMP-LEGAJO        PIC X(5).
          05 FILLER               PIC X(17).
          05 FILLER               PIC X(10) VALUE 'EMPLEADO: '.
          05 WS-EMP-NOMBRE        PIC X(30).
          05 FILLER               PIC X(4) VALUE SPACES.
          05 FILLER               PIC X VALUE '|'.

       01 WS-SUBT-GUIONES.
          05 FILLER               PIC X VALUE SPACES.
          05 FILLER               PIC X(3) VALUE ALL '-'.
          05 FILLER               PIC X(72) VALUE SPACES.
          05 FILLER               PIC X(3) VALUE ALL '-'.
          05 FILLER               PIC X VALUE SPACES.

       01 WS-SUBT-FECHAS.
          05 FILLER               PIC X VALUE '|'.
          05 FILLER               PIC X VALUE SPACE.
          05 FILLER               PIC X(18) VALUE
                                  'FECHA DE INGRESO: '.
          05 WS-ING-DIA           PIC 99.
          05 FILLER               PIC X VALUE '/'.
          05 WS-ING-MES           PIC 99.
          05 FILLER               PIC X VALUE '/'.
          05 WS-ING-ANIO          PIC 9(4).
          05 FILLER               PIC X(4) VALUE SPACES.
          05 FILLER               PIC X(21) VALUE
                                  'FECHA DE NACIMIENTO: '.
          05 WS-NAC-DIA           PIC 99.
          05 FILLER               PIC X VALUE '/'.
          05 WS-NAC-MES           PIC 99.
          05 FILLER               PIC X VALUE '/'.
          05 WS-NAC-ANIO          PIC 9(4).
          05 FILLER               PIC X(14) VALUE SPACES.
          05 FILLER               PIC X VALUE '|'.

       01 WS-HEAD-COLUMN.
          05 FILLER               PIC X VALUE '|'.
          05 FILLER               PIC X(7) VALUE SPACES.
          05 FILLER               PIC X(17) VALUE
                                  'FECHA DE LA VENTA'.
          05 FILLER               PIC X(7) VALUE SPACES.
          05 FILLER               PIC X VALUE '|'.
          05 FILLER               PIC X(14) VALUE SPACES.
          05 FILLER               PIC X(17) VALUE
                                  'MONTO DE LA VENTA'.
          05 FILLER               PIC X(15) VALUE SPACES.
          05 FILLER               PIC X VALUE '|'.

       01 WS-DATA-COLUMN.
          05 FILLER               PIC X VALUE '|'.
          05 FILLER               PIC X(10) VALUE SPACES.
          05 WS-VENT-DIA          PIC 99.
          05 FILLER               PIC X VALUE '/'.
          05 WS-VENT-MES          PIC 99.
          05 FILLER               PIC X VALUE '/'.
          05 WS-VENT-ANIO         PIC 9(4).
          05 FILLER               PIC X(11) VALUE SPACES.
          05 FILLER               PIC X VALUE '|'.
          05 FILLER               PIC X(35) VALUE SPACES.
          05 WS-VENT-MONTO        PIC $$$$$$9,99.
          05 FILLER               PIC X VALUE SPACE.
          05 FILLER               PIC X VALUE '|'.
       01 WS-VENTATOTAL.
          05 FILLER               PIC X VALUE '|'.
          05 FILLER               PIC X(26) VALUE SPACES.
          05 FILLER               PIC X(13) VALUE
                                  'VENTA TOTAL: '.
          05 WS-MONTO-TOTAL       PIC $$$$$$$$9,99.
          05 FILLER               PIC X(27) VALUE SPACES.
          05 FILLER               PIC X VALUE '|'.

       77 WS-LEGAJO-BUSCAR        PIC X(5).

       77 WS-CALCULAR-TOTAL       PIC 9(9)V99.

       77 WS-VALIDAR-FECHA        PIC X.

       77 WS-SVERFECH             PIC X(08) VALUE 'VERFECH'.

       PROCEDURE DIVISION.
       0100-MAIN-PROGRAM.
           PERFORM 0200-OPEN-FILE
           PERFORM 0300-BUSCAR-X-LEG
           PERFORM 0400-WRITE-HEADER
           PERFORM 0500-WRITE-DATA UNTIL FS-FICH-VENTA-END
           PERFORM 0600-WRITE-TOTAL
           PERFORM 0700-CLOSE-FILE
           PERFORM 0800-CLOSE-PROGRAM.

       0200-OPEN-FILE.
           OPEN INPUT FICH-VENDER
           OPEN INPUT FICH-VENTA
           OPEN OUTPUT FICH-REPORT.

       0300-BUSCAR-X-LEG.
           ACCEPT WS-LEGAJO-BUSCAR FROM SYSIN
           MOVE WS-LEGAJO-BUSCAR TO R-VENDER
           READ FICH-VENDER
           IF FS-FICH-VENDER-NE THEN
              DISPLAY "EL EMPLEADO NO EXISTE."
              PERFORM 0700-CLOSE-FILE
              PERFORM 0800-CLOSE-PROGRAM
           END-IF
           IF FS-FICH-VENDER NOT = 00 OR FS-FICH-VENDER NOT = 23
              DISPLAY "HA OCURRIDO UN ERROR EN LA LECTURA "
                      FS-FICH-VENDER
              PERFORM 0700-CLOSE-FILE
              PERFORM 0800-CLOSE-PROGRAM
           END-IF
           CALL WS-SVERFECH USING R-VENDER-NACIMIENTO, WS-VALIDAR-FECHA
           IF WS-VALIDAR-FECHA = 'N'
              DISPLAY "FECHA NACIMIENTO INVALIDA " R-VENDER-NACIMIENTO
                      " LEGAJO: " R-VENDER-LEGAJO
              PERFORM 0700-CLOSE-FILE
              PERFORM 0800-CLOSE-PROGRAM
           END-IF
           CALL WS-SVERFECH USING R-VENDER-INGRESO, WS-VALIDAR-FECHA
           IF WS-VALIDAR-FECHA = 'N'
              DISPLAY "FECHA INGRESO ERRONEA " R-VENDER-INGRESO
                      " LEGAJO: " R-VENDER-LEGAJO
              PERFORM 0700-CLOSE-FILE
              PERFORM 0800-CLOSE-PROGRAM
           END-IF
           MOVE R-VENDER-LEGAJO TO WS-EMP-LEGAJO
           MOVE R-VENDER-NOMBRE TO WS-EMP-NOMBRE
           MOVE R-V-I-DIA TO WS-ING-DIA
           MOVE R-V-I-MES TO WS-ING-MES
           MOVE R-V-I-ANIO TO WS-ING-ANIO
           MOVE R-V-N-DIA TO WS-NAC-DIA
           MOVE R-V-N-MES TO WS-NAC-MES
           MOVE R-V-N-ANIO TO WS-NAC-ANIO.

       0400-WRITE-HEADER.
           WRITE R-FICH-REPORT FROM WS-GUIONES
           WRITE R-FICH-REPORT FROM WS-TITULO
           WRITE R-FICH-REPORT FROM WS-GUIONES
           WRITE R-FICH-REPORT FROM WS-SUBT-LEG
           WRITE R-FICH-REPORT FROM WS-SUBT-GUIONES
           WRITE R-FICH-REPORT FROM WS-SUBT-FECHAS
           WRITE R-FICH-REPORT FROM WS-GUIONES
           WRITE R-FICH-REPORT FROM WS-GUIONES
           WRITE R-FICH-REPORT FROM WS-HEAD-COLUMN
           WRITE R-FICH-REPORT FROM WS-GUIONES.

       0500-WRITE-DATA.
           READ FICH-VENTA
           IF R-VENTA-LEGAJO = WS-LEGAJO-BUSCAR THEN
              MOVE VENTA-DIA TO WS-VENT-DIA
              MOVE VENTA-MES TO WS-VENT-MES
              MOVE VENTA-ANIO TO WS-VENT-ANIO
              MOVE R-VENTA-MONTO TO WS-VENT-MONTO
              WRITE R-FICH-REPORT FROM WS-DATA-COLUMN
              WRITE R-FICH-REPORT FROM WS-GUIONES
              ADD R-VENTA-MONTO TO WS-CALCULAR-TOTAL
           END-IF.

       0600-WRITE-TOTAL.
           MOVE WS-CALCULAR-TOTAL TO WS-MONTO-TOTAL
           WRITE R-FICH-REPORT FROM WS-GUIONES
           WRITE R-FICH-REPORT FROM WS-VENTATOTAL
           WRITE R-FICH-REPORT FROM WS-GUIONES.

       0700-CLOSE-FILE.
           CLOSE FICH-VENDER
           CLOSE FICH-VENTA
           CLOSE FICH-REPORT.

       0800-CLOSE-PROGRAM.
           STOP RUN.
