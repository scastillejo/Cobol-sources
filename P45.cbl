       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG-45.
       AUTHOR. NESTOR AMICO.
      ********************************************
      *  PROGRAMA NRO 45
      ********************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "S-ASCALU.CPY".   
       DATA DIVISION.
       FILE SECTION.
       COPY "F-ASCALU.CPY".		  
	   
       WORKING-STORAGE SECTION.

       01 DETALLE-01.
          03 D1-CODIGO   PIC 9(03).
          03 D1-APELLIDO PIC X(21).		  
          03 D1-NOMBRE   PIC X(21).

       01 W-DATOS.
          03 CODIGO   PIC 9(03).
          03 APELLIDO PIC X(21).		  
          03 NOMBRE   PIC X(21).
 
       01 W1-DATOS.
          03 W1-OPCION      PIC 9.
          03 W1-DESCRIPCION PIC X(18).
          03 W1-NUMERICO    PIC 9(08).
	   
       01 TABLA-W-DATOS.
          03 TAB-W-DATOS OCCURS 50 TIMES.
             05 T-CODIGO   PIC 9(03).
             05 T-APELLIDO PIC X(21).			 
             05 T-NOMBRE   PIC X(21).

       01 I  PIC 999.
       01 J  PIC 999.
       01 X  PIC 999 VALUE 4.
       01 F  PIC 999 VALUE 0.
       01 C  PIC 99.
       01 W-TITULO-ENC PIC X(60).
       01 M-ERROR      PIC X(60).
       01 W-OCCURS     PIC 99.

       01 SINO PIC X(001).
      ********************************************
       PROCEDURE DIVISION.         
      ********************************************
       INICIO.
           DISPLAY " " ERASE 
           MOVE 50 TO W-OCCURS
           MOVE "CONSULTA DE ALUMNOS A IMPORTAR" TO W-TITULO-ENC.
           PERFORM ENCABEZADO THRU ENCABEZADO-F.
           OPEN INPUT ASCALU
           PERFORM 100-LEO-00 THRU 100-LEO-99
           DISPLAY "INGRESE NRO. REGISTROS A VER:" LINE 5 POSITION 1.		   
           ACCEPT C LINE 5 POSITION 31.
           COMPUTE J = J - 1
           PERFORM MOS-00 THRU MOS-99 VARYING I FROM 1 BY 1
              UNTIL I > W-OCCURS.
           CLOSE ASCALU.		   
           ACCEPT SINO LINE 24 POSITION 80.
       FINAL-F.
           EXIT PROGRAM.
           STOP RUN.
		   
       100-LEO-00.
           COMPUTE J = J + 1
           READ ASCALU NEXT AT END
                GO TO 100-LEO-99
           END-READ
           MOVE REG-ASCALU TO TAB-W-DATOS(J).
           GO TO 100-LEO-00.
       100-LEO-99. EXIT.

       MOS-00.
           IF I > J
              COMPUTE I = W-OCCURS + 1
              GO TO MOS-99
           END-IF.

           IF F = 0
             DISPLAY " " ERASE
             MOVE "CONSULTA DE ALUMNOS A IMPORTAR" TO W-TITULO-ENC
             PERFORM ENCABEZADO THRU ENCABEZADO-F
           END-IF.

           COMPUTE X = X + 1
           COMPUTE F = F + 1

           MOVE T-CODIGO(I)   TO D1-CODIGO   
           MOVE T-APELLIDO(I) TO D1-APELLIDO
           MOVE T-NOMBRE(I)   TO D1-NOMBRE   

           DISPLAY DETALLE-01 LINE X POSITION 1.
	   
           IF (F = C) OR (I = J)
             IF I < J
               MOVE 0 TO F
               MOVE 4 TO X
               ACCEPT SINO LINE X POSITION 1
             ELSE
               COMPUTE I = I + W-OCCURS
             END-IF
           END-IF.
       MOS-99. EXIT.

       ENCABEZADO.
           DISPLAY "EMPRESA UNION S.A."  LINE 1 POSITION 1.
           INITIALIZE W1-DATOS.
           MOVE 3 TO W1-OPCION.
           CALL "P11" USING W1-DATOS.
           CANCEL "P11".
           DISPLAY W1-DESCRIPCION        LINE 1 POSITION 70
           DISPLAY W-TITULO-ENC          LINE 2 POSITION 1.
           INITIALIZE W1-DATOS.
           MOVE 8 TO W1-OPCION.
           CALL "P11" USING W1-DATOS.
           CANCEL "P11".
           DISPLAY W1-DESCRIPCION        LINE 2 POSITION 70.
       ENCABEZADO-F. EXIT.

       ERRORES.
           DISPLAY M-ERROR LINE 24 POSITION 1.
           ACCEPT SINO LINE 24 POSITION 80.
           INITIALIZE M-ERROR
           DISPLAY M-ERROR LINE 24 POSITION 1.
       ERRORES-F. EXIT.

