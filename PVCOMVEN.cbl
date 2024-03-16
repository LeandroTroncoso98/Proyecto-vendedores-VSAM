       IDENTIFICATION DIVISION.
       PROGRAM-ID. PVCOMVEN.
       AUTHOR. TRONCOSO LEANDRO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-EMPLEADOS ASSIGN TO EMPLEADO
           ORGANIZATION IS INDEXED
           ACCESS IS SEQUENTIAL
           RECORD KEY IS R-EMP-LEGAJO
           FILE STATUS IS FS-EMPLEADOS.

           SELECT F-VENTAS ASSIGN TO VENTAS
           ORGANIZATION IS SEQUENTIAL
           ACCESS IS SEQUENTIAL
           FILE STATUS IS FS-VENTAS.

           SELECT F-RESUMEN ASSIGN TO RESUMEN
           ORGANIZATION IS SEQUENTIAL
           ACCESS IS SEQUENTIAL
           FILE STATUS IS FS-RESUMEN.


       DATA DIVISION.
       FILE SECTION.

       FD F-EMPLEADOS DATA RECORD IS R-EMP.
       01 R-EMP.
          05 R-EMP-LEGAJO      PIC X(5).
          05 R-EMP-NOMBRE      PIC X(30).
          05 R-EMP-NACIMIENTO.
             10 R-E-N-DIA         PIC 9(2).
             10 R-E-N-MES         PIC 9(2).
             10 R-E-N-ANIO        PIC 9(4).
          05 R-EMP-INGRESO.
             10 R-E-I-DIA         PIC 9(2).
             10 R-E-I-MES         PIC 9(2).
             10 R-E-I-ANIO        PIC 9(4).
          05 R-EMP-CIUDAD      PIC X(26).

       FD F-VENTAS RECORDING MODE IS F
                   DATA RECORD IS R-VENTA.
       01 R-VENTA.
          05 R-VENTA-LEGAJO       PIC X(5).
          05 R-VENTA-MONTO        PIC 9(6)V99.
          05 R-VENTA-FECHA.
             10 VENTA-DIA         PIC 99.
             10 VENTA-MES         PIC 99.
             10 VENTA-ANIO        PIC 9(4).
       FD F-RESUMEN RECORDING MODE IS F
                    DATA RECORD IS R-RESUMEN.
       01 R-RESUMEN               PIC X(80).

       WORKING-STORAGE SECTION.
       01 FS-EMPLEADOS            PIC 99.
          88 FS-EMPLEADOS-END        VALUE 10.
          88 FS-EMPLEADOS-OK         VALUE 00.

       01 FS-VENTAS               PIC 99.
          88 FS-VENTAS-END           VALUE 10.
          88 FS-VENTAS-OK            VALUE 00.

       01 FS-RESUMEN              PIC 99.

       77 WS-VALIDAR              PIC X.

       01 WS-GUIONES.
          05 FILLER               PIC X VALUE SPACE.
          05 FILLER               PIC X(78) VALUE ALL '-'.
          05 FILLER               PIC X VALUE SPACE.

       01 WS-TITULO.
          05 FILLER               PIC X VALUE '|'.
          05 FILLER               PIC X(36) VALUE  SPACES.
          05 FILLER               PIC X(7) VALUE 'RESUMEN'.
          05 FILLER               PIC X(35) VALUE SPACES.
          05 FILLER               PIC X VALUE '|'.

       01 WS-SUBTITULO.
          05 FILLER               PIC X VALUE '|'.
          05 FILLER               PIC X VALUE SPACE.
          05 FILLER               PIC X(18) VALUE
                                  'NOMBRE DE EMPLEADO'.
          05 FILLER               PIC X(13) VALUE SPACES.
          05 FILLER               PIC X VALUE '|'.
          05 FILLER               PIC X VALUE SPACE.
          05 FILLER               PIC X(11) VALUE 'CANT.VENTAS'.
          05 FILLER               PIC X VALUE SPACE.
          05 FILLER               PIC X VALUE '|'.
          05 FILLER               PIC X VALUE SPACE.
          05 FILLER               PIC X(11) VALUE 'VALOR TOTAL'.
          05 FILLER               PIC X(5) VALUE SPACES.
          05 FILLER               PIC X VALUE '|'.
          05 FILLER               PIC X VALUE SPACE.
          05 FILLER               PIC X(8) VALUE 'COMISION'.
          05 FILLER               PIC X(4) VALUE SPACES.
          05 FILLER               PIC X VALUE '|'.

       01 WS-DATOS.
          05 FILLER               PIC X VALUE '|'.
          05 FILLER               PIC X VALUE SPACE.
          05 WS-EMP-NOMBRE        PIC X(30).
          05 FILLER               PIC X VALUE SPACE.
          05 FILLER               PIC X VALUE '|'.
          05 FILLER               PIC X VALUE SPACE.
          05 WS-EMP-VENTAS        PIC 9(11).
          05 FILLER               PIC X VALUE SPACE.
          05 FILLER               PIC X VALUE '|'.
          05 FILLER               PIC X VALUE SPACE.
          05 WS-EMP-TOTAL         PIC $$$$$$$9,99.
          05 FILLER               PIC X(5) VALUE SPACES.
          05 FILLER               PIC X VALUE '|'.
          05 FILLER               PIC X VALUE SPACE.
          05 WS-EMP-COMISION      PIC $$$$$$$9,99.
          05 FILLER               PIC X VALUE SPACE.
          05 FILLER               PIC X VALUE '|'.

       01 WS-RESUMEN-GRAL.
          05 FILLER               PIC X VALUE '|'.
          05 FILLER               PIC X VALUE SPACE.
          05 FILLER               PIC X(30) VALUE
                                  'CANTIDAD DE EMPLEADOS LEIDOS: '.
          05 WS-CANT-EMP          PIC 9(4).
          05 FILLER               PIC X(10) VALUE SPACES.
          05 FILLER               PIC X(28) VALUE
                                  'CANTIDAD DE VENTAS TOTALES: '.
          05 WS-CANT-VENT         PIC 9(4).
          05 FILLER               PIC X VALUE SPACE.
          05 FILLER               PIC X VALUE '|'.

          77 WS-CALCULAR-TOTAL    PIC 9(7)V99.

          77 WS-CALCULAR-COMIS    PIC 9(7)V99.

          77 WS-SVERFECH          PIC X(08) VALUE 'VERFECH'.


       PROCEDURE DIVISION.
       0100-INIT-PROGRAM.
           PERFORM 0200-OPEN-FILE
           PERFORM 0300-WRITE-HEADER
           PERFORM 0400-READ-FILES
           PERFORM 0500-COMPARE-LEG UNTIL FS-EMPLEADOS-END
                                   OR FS-VENTAS-END
           PERFORM 0510-WRITE-DATA
           PERFORM 0600-WRITE-END
           PERFORM 0700-CLOSE-FILES
           PERFORM 0800-CLOSE-PROGRAM.

       0200-OPEN-FILE.
           OPEN INPUT F-EMPLEADOS
           OPEN INPUT F-VENTAS
           OPEN OUTPUT F-RESUMEN.

       0300-WRITE-HEADER.
           WRITE R-RESUMEN FROM WS-GUIONES
           WRITE R-RESUMEN FROM WS-TITULO
           WRITE R-RESUMEN FROM WS-GUIONES
           WRITE R-RESUMEN FROM WS-SUBTITULO
           WRITE R-RESUMEN FROM WS-GUIONES.

       0400-READ-FILES.
           READ F-EMPLEADOS
           READ F-VENTAS.

       0500-COMPARE-LEG.
           IF R-EMP-LEGAJO = R-VENTA-LEGAJO THEN
              CALL WS-SVERFECH USING R-VENTA-FECHA, WS-VALIDAR
              IF WS-VALIDAR = "S" THEN
                 ADD R-VENTA-MONTO TO WS-CALCULAR-TOTAL
                 ADD 1 TO WS-EMP-VENTAS
                 ADD 1 TO WS-CANT-VENT
              ELSE
                 DISPLAY "FECHA ERRONEA:" R-VENTA-FECHA
                         " LEGAJO:" R-EMP-LEGAJO
              END-IF
                 READ F-VENTAS
           ELSE
              IF R-EMP-LEGAJO > R-VENTA-LEGAJO THEN
                 READ F-VENTAS
              ELSE
                 PERFORM 0510-WRITE-DATA
              END-IF
           END-IF.

       0510-WRITE-DATA.
           COMPUTE
            WS-CALCULAR-COMIS = WS-CALCULAR-TOTAL * 0,13
           END-COMPUTE
           ADD 1 TO WS-CANT-EMP
           MOVE R-EMP-NOMBRE TO WS-EMP-NOMBRE
           MOVE WS-CALCULAR-TOTAL TO WS-EMP-TOTAL
           MOVE WS-CALCULAR-COMIS TO WS-EMP-COMISION
           WRITE R-RESUMEN FROM WS-DATOS
           READ F-EMPLEADOS
           IF FS-EMPLEADOS NOT = 00 OR FS-EMPLEADOS NOT = 10
              DISPLAY "ERROR EN FICHERO VSAM: " FS-EMPLEADOS
              PERFORM 0700-CLOSE-FILES
              PERFORM 0800-CLOSE-PROGRAM
           END-IF
           MOVE 0 TO WS-EMP-VENTAS.

       0600-WRITE-END.
           WRITE R-RESUMEN FROM WS-GUIONES
           WRITE R-RESUMEN FROM WS-RESUMEN-GRAL
           WRITE R-RESUMEN FROM WS-GUIONES.

       0700-CLOSE-FILES.
           CLOSE F-EMPLEADOS
           CLOSE F-VENTAS
           CLOSE F-RESUMEN.

       0800-CLOSE-PROGRAM.
           STOP RUN.
