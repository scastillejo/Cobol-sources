       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG-47.
       AUTHOR. NESTOR AMICO.
      ********************************************
      *  PROGRAMA NRO 47
      ********************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "S-PROVIN.CPY".
       COPY "S-ALUMNO.CPY".
       COPY "S-CURSO.CPY".
       DATA DIVISION.
       FILE SECTION.
       COPY "F-PROVIN.CPY".
       COPY "F-ALUMNO.CPY".
       COPY "F-CURSO.CPY".
	   
       WORKING-STORAGE SECTION.

       01 TITULO-01.
          03 PIC X(04) VALUE "CURS".
          03 PIC X. 
          03 PIC X(03) VALUE "COD".
          03 PIC X. 
          03 PIC X(20) VALUE "APELLIDO".
          03 PIC X. 
          03 PIC X(20) VALUE "NOMBRE".
          03 PIC X. 
          03 PIC X(04) VALUE "PROV".

       01 DETALLE-01.
          03 D1-CODCUR      PIC X(04).
          03                PIC X. 
          03 D1-CODALUM     PIC 9(03).
          03                PIC X. 
          03 D1-ALUAPE      PIC X(20).
          03                PIC X. 		  
          03 D1-ALUNOM      PIC X(20).
          03                PIC X. 
          03 D1-CODPROV     PIC X(04).            

       01 W1-DATOS.
          03 W1-OPCION      PIC 9.
          03 W1-DESCRIPCION PIC X(18).
          03 W1-NUMERICO    PIC 9(08).
	   
       01 UBICACIONFILA PIC 999 VALUE 7.
       01 FILA          PIC 999 VALUE 0.
       01 CANTPEDIDA    PIC 99.

       01 W-TITULO-ENC   PIC X(60).
       01 OPCION         PIC 99.
       01 M-ERROR        PIC X(60).
       01 ST-PROVINCIAS  PIC XX.
       01 ST-ALUMNOS     PIC XX.
       01 ST-CURSOS      PIC XX.
	   
       01 SINO PIC X(001).
      ********************************************
       PROCEDURE DIVISION.         
      ********************************************
       DECLARATIVES.
       DECLA SECTION.
       USE AFTER STANDARD ERROR PROCEDURE ON PROVINCIAS.
       END DECLARATIVES.

       UNION SECTION.
       INICIO.
           PERFORM 400-CONSULTA-00 THRU 400-CONSULTA-99.
           ACCEPT SINO LINE 24 POSITION 80.
       FINAL-F.
           EXIT PROGRAM.
           STOP RUN.

       400-CONSULTA-00.
           DISPLAY " " ERASE
           MOVE "CONSULTA POR CURSOS" TO W-TITULO-ENC
           PERFORM ENCABEZADO THRU ENCABEZADO-F.
           DISPLAY "INGRESE CANTIDAD REGISTROS:" LINE 5 POSITION 1.
           ACCEPT CANTPEDIDA LINE 5 POSITION 30.
           OPEN INPUT PROVINCIAS.
           OPEN INPUT ALUMNOS.
           OPEN INPUT CURSOS.
           MOVE 6 TO UBICACIONFILA.
       400-CONSULTA-10.
           INITIALIZE REG-ALUMNO
           START ALUMNOS KEY NOT LESS ALU-CODIGO-CURSO
              INVALID KEY
                 GO TO 400-CONSULTA-90
           END-START.
       400-CONSULTA-20.
         
           READ ALUMNOS NEXT AT END
                GO TO 400-CONSULTA-90
           END-READ.

           MOVE ALU-CODIGO-CURSO TO CUR-CODIGO                       
           READ CURSOS KEY IS CUR-CODIGO
              INVALID KEY
                  CONTINUE
              NOT INVALID KEY
                  MOVE CUR-ABREVIATURA TO D1-CODCUR
           END-READ 

           MOVE ALU-CODIGO-PROVINCIA TO PRO-CODIGO                       
           READ PROVINCIAS KEY IS PRO-CODIGO
              INVALID KEY
                  CONTINUE
              NOT INVALID KEY
                  MOVE PRO-ABREVIATURA TO D1-CODPROV
           END-READ 

           IF FILA = 0
              DISPLAY " " ERASE
              MOVE "CONSULTA DE PROVINCIAS" TO W-TITULO-ENC
              PERFORM ENCABEZADO THRU ENCABEZADO-F
              PERFORM COLUMNAS THRU COLUMNAS-F
           END-IF

           COMPUTE UBICACIONFILA = UBICACIONFILA + 1
           COMPUTE FILA = FILA + 1
   
           MOVE ALU-CODIGO      TO D1-CODALUM                          
           MOVE ALU-APELLIDO    TO D1-ALUAPE   
           MOVE ALU-NOMBRE      TO D1-ALUNOM
			   
           DISPLAY DETALLE-01 LINE UBICACIONFILA POSITION 1

           IF FILA = CANTPEDIDA
              MOVE 0 TO FILA
              MOVE 6 TO UBICACIONFILA
              ACCEPT SINO LINE 24 POSITION 1
           END-IF                           
           GO TO 400-CONSULTA-20.
       400-CONSULTA-90.
           CLOSE ALUMNOS.
           CLOSE PROVINCIAS.
           CLOSE CURSOS.		   
       400-CONSULTA-99. EXIT.

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

       COLUMNAS.
           DISPLAY TITULO-01 LINE 5 POSITION 1.
       COLUMNAS-F. EXIT.

       ERRORES.
           DISPLAY M-ERROR LINE 24 POSITION 1.
           ACCEPT SINO LINE 24 POSITION 80.
           INITIALIZE M-ERROR.
           DISPLAY M-ERROR LINE 24 POSITION 1.
       ERRORES-F. EXIT.

