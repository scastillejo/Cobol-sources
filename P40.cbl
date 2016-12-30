       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG-40.
       AUTHOR. NESTOR AMICO.
      ********************************************
      *  PROGRAMA NRO 40
      ********************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
	   
       WORKING-STORAGE SECTION.
               
       01 W1-DATOS.
          03 W1-OPCION      PIC 9.
          03 W1-DESCRIPCION PIC X(18).
          03 W1-NUMERICO    PIC 9(08).
	   
       01 W-TITULO-ENC PIC X(60).
       01 OPCION       PIC 99.
	   
       01 SINO PIC X(001).
      ********************************************
       PROCEDURE DIVISION.         
      ********************************************
         INICIO.
           DISPLAY " " ERASE 
           MOVE "MENU GENERAL" TO W-TITULO-ENC.
           PERFORM ENCABEZADO THRU ENCABEZADO-F.
           PERFORM PANTALLAINICIO THRU PANTALLAINICIO-F.
           ACCEPT OPCION LINE 15 POSITION 28.

           IF OPCION = 1
              CALL "P41"
              CANCEL "P41"
              GO TO INICIO
           END-IF.
           IF OPCION = 2
              CALL "P42"
              CANCEL "P42"
              GO TO INICIO
           END-IF. 
           IF OPCION = 3
              CALL "P43"
              CANCEL "P43"
              GO TO INICIO
           END-IF.
           IF OPCION = 4
              CALL "P44"
              CANCEL "P44"		   
              GO TO INICIO
           END-IF.
           IF OPCION = 5
              CALL "P45"
              CANCEL "P45"		   
              GO TO INICIO
           END-IF.
           IF OPCION = 6
              CALL "P46"
              CANCEL "P46"		   
              GO TO INICIO
           END-IF.
           IF OPCION = 7
              CALL "P47"
              CANCEL "P47"		   
              GO TO INICIO
           END-IF.
           IF OPCION = 8
              GO TO INICIO
           END-IF.
           IF OPCION = 9
              GO TO FINAL-F 
           END-IF.		   

           ACCEPT SINO LINE 24 POSITION 80.
       FINAL-F.
           EXIT PROGRAM.
           STOP RUN.

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

       PANTALLAINICIO.
           DISPLAY "1. ABMC de Provincias"      LINE 5  POSITION 1.
           DISPLAY "2. ABMC de Alumnos"         LINE 6  POSITION 1.
           DISPLAY "3. ABMC de Cursos"          LINE 7  POSITION 1.
           DISPLAY "4. Importacion de alumnos"  LINE 8  POSITION 1.
           DISPLAY "5. Consulta de alumnos para importar"
                       LINE 9  POSITION 1.
           DISPLAY "6. Consulta de alumnos por provincias"
                       LINE 10  POSITION 1.
           DISPLAY "7. Consulta de alumnos por cursos"
                       LINE 11  POSITION 1.
           DISPLAY "8. Parcial" LINE 12  POSITION 1.
           DISPLAY "OPCION:"    LINE 15 POSITION 20.
           DISPLAY "Salir = 9"  LINE 16 POSITION 70.
       PANTALLAINICIO-F. EXIT.
	   
  
