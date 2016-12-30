       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG-44.
       AUTHOR. NESTOR AMICO.
      ********************************************
      *  PROGRAMA NRO 44
      ********************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "S-ASCALU.CPY". 
       COPY "S-ALUMNO.CPY".	   
       DATA DIVISION.
       FILE SECTION.
       COPY "F-ASCALU.CPY".	
       COPY "F-ALUMNO.CPY".	
	   
       WORKING-STORAGE SECTION.

       01 W1-DATOS.
          03 W1-OPCION      PIC 9.
          03 W1-DESCRIPCION PIC X(18).
          03 W1-NUMERICO    PIC 9(08).
	   
       01 W-TITULO-ENC PIC X(60).
       01 ST-ALUMNOS   PIC XX.
       01 SINO PIC X(001).

       01 W-GRABO      PIC 999.
       01 W-CUENTA     PIC 999.
	   
      ********************************************
       PROCEDURE DIVISION.         
      ********************************************
       DECLARATIVES.
       DECLA SECTION.
       USE AFTER STANDARD ERROR PROCEDURE ON ALUMNOS.
       END DECLARATIVES.

       UNION SECTION.
       INICIO.
           DISPLAY " " ERASE 
           MOVE "IMPORTACION DE ALUMNOS" TO W-TITULO-ENC.
           PERFORM ENCABEZADO THRU ENCABEZADO-F.
           OPEN INPUT ASCALU
           OPEN I-O ALUMNOS

           IF ST-ALUMNOS = "35"
             OPEN OUTPUT ALUMNOS
             CLOSE ALUMNOS
             OPEN I-O ALUMNOS
           END-IF  

           INITIALIZE W-GRABO
                      W-CUENTA
           PERFORM 100-LEO-00 THRU 100-LEO-99
           CLOSE ASCALU
                 ALUMNOS.

           ACCEPT SINO LINE 24 POSITION 80.
       FINAL-F.
           EXIT PROGRAM.
           STOP RUN.
		   
       100-LEO-00.
           READ ASCALU NEXT AT END
                GO TO 100-LEO-99
           END-READ
		   
           MOVE R-CODIGO TO ALU-CODIGO
           MOVE R-APELLIDO(2:20) TO ALU-APELLIDO
           MOVE R-NOMBRE(2:20) TO ALU-NOMBRE
           MOVE 1 TO ALU-CODIGO-CURSO
           MOVE 1 TO ALU-CODIGO-PROVINCIA
		   
           WRITE REG-ALUMNO
              INVALID KEY
                ADD 1 TO  W-CUENTA
                DISPLAY "TOTAL REGISTROS: " LINE 6 POSITION 1
                DISPLAY W-CUENTA LINE 6 POSITION 25
              NOT INVALID KEY
                ADD 1 TO  W-GRABO
                DISPLAY "REGISTROS GRABADOS: " LINE 5 POSITION 1 
                DISPLAY W-GRABO LINE 5 POSITION 25
           END-WRITE 
           GO TO 100-LEO-00.
       100-LEO-99. EXIT.
   
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


