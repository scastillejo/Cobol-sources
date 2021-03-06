       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG-11.
       AUTHOR. NESTOR AMICO.
      ********************************************
      *  PROGRAMA NRO 11
      ********************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 FECHACOMPLETA.
          03 FECHASS PIC 9(02) VALUE 20.
          03 FECHASISTEMA.
            05 FECHAAA PIC 9(02).
            05 FECHAMM PIC 9(02).
            05 FECHADD PIC 9(02).
			
       01 HORASISTEMA.
          03 HH PIC 9(02).
          03 MM PIC 9(02).
          03 SS PIC 9(02).
			
       01 TABLA-MES.
           03 PIC X(12) VALUE "ENERO     07".
           03 PIC X(12) VALUE "FEBRERO   09".
           03 PIC X(12) VALUE "MARZO     07".
           03 PIC X(12) VALUE "ABRIL     07".
           03 PIC X(12) VALUE "MAYO      06".
           03 PIC X(12) VALUE "JUNIO     07".
           03 PIC X(12) VALUE "JULIO     07".
           03 PIC X(12) VALUE "AGOSTO    08".
           03 PIC X(12) VALUE "SEPTIEMBRE12".
           03 PIC X(12) VALUE "OCTUBRE   09".
           03 PIC X(12) VALUE "NOVIEMBRE 11".
           03 PIC X(12) VALUE "DICIEMBRE 11".
       01 TABLA-MES-R REDEFINES TABLA-MES.
           03 TAB-MES OCCURS 12 TIMES PIC X(12).

       01 MESES.
           03 MESNOMBRE PIC X(10).
           03 MESNUM    PIC 9(02).

       01 SINO PIC X(001).
	   
       LINKAGE SECTION.
       01 W1-DATOS.
          03 W1-OPCION      PIC 9.
          03 W1-DESCRIPCION PIC X(18).
          03 W1-NUMERICO    PIC 9(08).

      ********************************************
       PROCEDURE DIVISION USING W1-DATOS.
      ********************************************
       INICIO.
           IF W1-OPCION < 7
              ACCEPT FECHASISTEMA FROM DATE
              IF W1-NUMERICO not = 0
                 MOVE W1-NUMERICO TO FECHAcompleta
              END-IF
              IF (W1-OPCION = 1) OR (W1-OPCION = 2) 
                 MOVE FECHASISTEMA TO W1-NUMERICO
                 IF W1-OPCION = 2
                   MOVE FECHACOMPLETA TO W1-NUMERICO
                 END-IF
              END-IF
              IF (W1-OPCION = 3) OR (W1-OPCION = 4)
                 MOVE FECHADD TO W1-DESCRIPCION(1:2)
                 MOVE "/" TO W1-DESCRIPCION(3:1)
                 MOVE FECHAMM TO W1-DESCRIPCION(4:2)
                 MOVE "/" TO W1-DESCRIPCION(6:1)
                 MOVE FECHAAA TO W1-DESCRIPCION(7:2)
                 IF W1-OPCION = 4
                    MOVE FECHASS TO W1-DESCRIPCION(7:2)
                    MOVE FECHAAA TO W1-DESCRIPCION(9:2)
                 END-IF
              END-IF 
              IF W1-OPCION > 4
                 MOVE FECHADD TO W1-DESCRIPCION(1:2)
                 MOVE " " TO W1-DESCRIPCION(3:1)
                 MOVE TAB-MES(FECHAMM) TO MESES
                 IF W1-OPCION = 5
                    MOVE MESNOMBRE TO W1-DESCRIPCION(4:3)
                    MOVE FECHASS TO W1-DESCRIPCION(8:2)
                    MOVE FECHAAA TO W1-DESCRIPCION(10:2)
                 ELSE 
                    MOVE MESNOMBRE TO W1-DESCRIPCION(4:MESNUM)
                    MOVE FECHASS TO W1-DESCRIPCION(MESNUM + 4:2)
                    MOVE FECHAAA TO W1-DESCRIPCION(MESNUM + 6:2)
                 END-IF
              END-IF
           END-IF
           IF W1-OPCION > 6 
              ACCEPT HORASISTEMA FROM TIME
              IF W1-OPCION = 7
                 MOVE HORASISTEMA TO W1-NUMERICO
              ELSE 
                 MOVE HH TO W1-DESCRIPCION(1:2)
                 MOVE ":" TO W1-DESCRIPCION(3:1)
                 MOVE MM TO W1-DESCRIPCION(4:2)
                 MOVE ":" TO W1-DESCRIPCION(6:1)
                 MOVE SS TO W1-DESCRIPCION(7:2)
              END-IF
           END-IF.
         FINAL-F.
           EXIT PROGRAM.
           STOP RUN.
