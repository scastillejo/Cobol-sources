       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG-41.
       AUTHOR. NESTOR AMICO.
      ********************************************
      *  PROGRAMA NRO 41
      ********************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "S-PROVIN.CPY".
       COPY "S-ALUMNO.CPY".
       DATA DIVISION.
       FILE SECTION.
       COPY "F-PROVIN.CPY".
       COPY "F-ALUMNO.CPY".
	   
       WORKING-STORAGE SECTION.

       01 TITULO-01.
          03 PIC X(02) VALUE "CD".
          03 PIC X. 
          03 PIC X(30) VALUE "DESCRIPCION".
          03 PIC X. 
          03 PIC X(04) VALUE "ABRV".

       01 DETALLE-01.
          03 D1-CODIGO      PIC 9(02).
          03                PIC X. 
          03 D1-DESCRIPCION PIC X(30).
          03                PIC X. 
          03 D1-ABREVIATURA PIC X(04).

       01 W-DATOS.
          03 CODIGO      PIC 9(02).
          03 DESCRIPCION PIC X(30).
          03 ABREVIATURA PIC X(04).
		  
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
       01 EXISTE         PIC X.
       01 CONF           PIC X.
	   
       01 SINO PIC X(001).
      ********************************************
       PROCEDURE DIVISION.         
      ********************************************
       DECLARATIVES.
       DECLA SECTION.
       USE AFTER STANDARD ERROR PROCEDURE ON PROVINCIAS.
       END DECLARATIVES.

       UNION SECTION.
       INICIO-1.
           OPEN I-O PROVINCIAS.

           IF ST-PROVINCIAS = "35"
             OPEN OUTPUT PROVINCIAS
             CLOSE PROVINCIAS
             OPEN I-O PROVINCIAS
           END-IF.  

       INICIO.
           DISPLAY " " ERASE 
           MOVE "ABMC DE PROVINCIAS" TO W-TITULO-ENC.
           PERFORM ENCABEZADO THRU ENCABEZADO-F.
           PERFORM PANTALLAINICIO THRU PANTALLAINICIO-F.
           ACCEPT OPCION LINE 12 POSITION 10.

           IF OPCION = 1
              PERFORM 100-ALTA-00 THRU 100-ALTA-99
              GO TO INICIO
           END-IF.
           IF OPCION = 2
              PERFORM 200-BAJA-00 THRU 200-BAJA-99
              GO TO INICIO
           END-IF. 
           IF OPCION = 3
              PERFORM 300-MODIFICACION-00 THRU 300-MODIFICACION-99
              GO TO INICIO
           END-IF.
           IF OPCION = 4
              INITIALIZE FILA UBICACIONFILA
              PERFORM 400-CONSULTA-00 THRU 400-CONSULTA-99  
              ACCEPT SINO LINE UBICACIONFILA POSITION 1    
              GO TO INICIO
           END-IF.
           IF OPCION = 9
              GO TO FINAL-F 
           END-IF.		   

           ACCEPT SINO LINE 24 POSITION 80.
       FINAL-F.
           CLOSE PROVINCIAS.
           EXIT PROGRAM.
           STOP RUN.

       100-LEO-00.
           READ PROVINCIAS KEY IS PRO-CODIGO
              INVALID KEY
                MOVE "N" TO EXISTE
              NOT INVALID KEY
                MOVE "S" TO EXISTE
           END-READ.
       100-LEO-99. EXIT.
	   
       100-LEOALUMNOS-00.
           OPEN I-O ALUMNOS.
           READ ALUMNOS KEY IS ALU-CODIGO-PROVINCIA
              INVALID KEY
                MOVE "N" TO EXISTE
              NOT INVALID KEY
                MOVE "S" TO EXISTE
           END-READ.
           CLOSE ALUMNOS.
       100-LEOALUMNOS-99. EXIT.
	   
       100-ALTA-00.
           DISPLAY " " ERASE
           MOVE "ALTA DE PROVINCIAS" TO W-TITULO-ENC
           PERFORM ENCABEZADO THRU ENCABEZADO-F
           PERFORM PANTALLA THRU PANTALLA-F.
       100-ALTA-10.
           ACCEPT CODIGO LINE 5 POSITION 13 PROMPT.
           IF CODIGO = 0
              GO TO 100-ALTA-99
           END-IF.
           MOVE CODIGO TO PRO-CODIGO
           PERFORM 100-LEO-00 THRU 100-LEO-99
           IF EXISTE = "S"
             MOVE "EL CODIGO YA EXISTE." TO M-ERROR
             PERFORM ERRORES THRU ERRORES-F
             GO TO 100-ALTA-10
           END-IF.
       100-ALTA-20.
           ACCEPT DESCRIPCION LINE 6 POSITION 13 PROMPT.
           IF DESCRIPCION = SPACES
             MOVE "LA DESCRIPCION NO PUEDE SER NULA." TO M-ERROR
             PERFORM ERRORES THRU ERRORES-F
             GO TO 100-ALTA-20
           END-IF.
       100-ALTA-30.
           ACCEPT ABREVIATURA LINE 7 POSITION 13 PROMPT.
           IF ABREVIATURA = SPACES
             MOVE "LA ABREVIATURA NO PUEDE SER NULA." TO M-ERROR
             PERFORM ERRORES THRU ERRORES-F
             GO TO 100-ALTA-30
           END-IF.
       100-ALTA-50.   
           ACCEPT CONF LINE 10 POSITION 18.
           IF (CONF = "S") OR (CONF = "N")
               IF CONF = "S"
                  MOVE W-DATOS TO REG-PROVINCIAS
                  WRITE REG-PROVINCIAS
                     INVALID KEY
                        MOVE "NO SE GRABO" TO M-ERROR
                     NOT INVALID KEY
                        MOVE "DATOS INGRESADOS" TO M-ERROR
                  END-WRITE
                  PERFORM ERRORES THRU ERRORES-F
                  GO TO 100-ALTA-00
               ELSE
                  GO TO 100-ALTA-10
               END-IF
           ELSE
             GO TO 100-ALTA-50             
           END-IF. 
       100-ALTA-99. EXIT.		   

       200-BAJA-00.
           DISPLAY " " ERASE
           MOVE "BAJA DE PROVINCIAS" TO W-TITULO-ENC
           PERFORM ENCABEZADO THRU ENCABEZADO-F
           PERFORM PANTALLA THRU PANTALLA-F.
       200-BAJA-10.
           ACCEPT CODIGO LINE 5 POSITION 13 PROMPT.
           IF CODIGO = 0
              GO TO 200-BAJA-99
           END-IF.
           MOVE CODIGO TO PRO-CODIGO
           PERFORM 100-LEO-00 THRU 100-LEO-99
           IF EXISTE = "N"
             MOVE "EL CODIGO NO EXISTE." TO M-ERROR
             PERFORM ERRORES THRU ERRORES-F
             GO TO 200-BAJA-10
           END-IF.
           MOVE CODIGO TO ALU-CODIGO-PROVINCIA
           PERFORM 100-LEOALUMNOS-00 THRU 100-LEOALUMNOS-99
           IF EXISTE = "S"
             MOVE "LA PROVINCIA SE ENCUENTRA EN ALUMNOS." TO M-ERROR
             PERFORM ERRORES THRU ERRORES-F
             GO TO 200-BAJA-10
           END-IF.
       200-BAJA-20.
           MOVE REG-PROVINCIAS TO W-DATOS
           DISPLAY DESCRIPCION LINE 6 POSITION 13
           DISPLAY ABREVIATURA LINE 7 POSITION 13.
       200-BAJA-50.   
           ACCEPT CONF LINE 10 POSITION 18.
           IF (CONF = "S") OR (CONF = "N")
               IF CONF = "S"
                  MOVE W-DATOS TO REG-PROVINCIAS
                  DELETE PROVINCIAS
                     INVALID KEY
                        MOVE "NO SE BORRO" TO M-ERROR
                     NOT INVALID KEY
                        MOVE "BORRADO EXITOSO" TO M-ERROR
                  END-DELETE
                  PERFORM ERRORES THRU ERRORES-F
                  GO TO 200-BAJA-00
               ELSE
                  GO TO 200-BAJA-10
               END-IF
           ELSE
             GO TO 200-BAJA-50             
           END-IF. 
       200-BAJA-99. EXIT.                  

       300-MODIFICACION-00.
           DISPLAY " " ERASE
           MOVE "MODIFICACION DE PROVINCIAS" TO W-TITULO-ENC
           PERFORM ENCABEZADO THRU ENCABEZADO-F
           PERFORM PANTALLA THRU PANTALLA-F.
       300-MODIFICACION-10.
           ACCEPT CODIGO LINE 5 POSITION 13 PROMPT.
           IF CODIGO = 0
              GO TO 300-MODIFICACION-99
           END-IF.
           MOVE CODIGO TO PRO-CODIGO
           PERFORM 100-LEO-00 THRU 100-LEO-99
           IF EXISTE = "N"
             MOVE "EL CODIGO NO EXISTE." TO M-ERROR
             PERFORM ERRORES THRU ERRORES-F
             GO TO 300-MODIFICACION-10
           END-IF.
       300-MODIFICACION-15.
           MOVE REG-PROVINCIAS TO W-DATOS
           DISPLAY DESCRIPCION LINE 6 POSITION 13
           DISPLAY ABREVIATURA LINE 7 POSITION 13.
       300-MODIFICACION-20.
           ACCEPT DESCRIPCION LINE 6 POSITION 13 PROMPT UPDATE.
           IF DESCRIPCION = SPACES
             MOVE "LA DESCRIPCION NO PUEDE SER NULA." TO M-ERROR
             PERFORM ERRORES THRU ERRORES-F
             GO TO 300-MODIFICACION-20
           END-IF.
       300-MODIFICACION-30.
           ACCEPT ABREVIATURA LINE 7 POSITION 13 PROMPT UPDATE.
           IF ABREVIATURA = SPACES
             MOVE "LA ABREVIATURA NO PUEDE SER NULA." TO M-ERROR
             PERFORM ERRORES THRU ERRORES-F
             GO TO 300-MODIFICACION-30
           END-IF.
       300-MODIFICACION-50.   
           ACCEPT CONF LINE 10 POSITION 18.
           IF (CONF = "S") OR (CONF = "N")
               IF CONF = "S"
                  MOVE W-DATOS TO REG-PROVINCIAS
                  REWRITE REG-PROVINCIAS
                     INVALID KEY
                        MOVE "NO SE MODIFICO" TO M-ERROR
                     NOT INVALID KEY
                        MOVE "DATOS MODIFICADOS" TO M-ERROR
                  END-REWRITE
                  PERFORM ERRORES THRU ERRORES-F
                  GO TO 300-MODIFICACION-00
               ELSE
                  GO TO 300-MODIFICACION-10
               END-IF
           ELSE
             GO TO 300-MODIFICACION-50             
           END-IF. 
       300-MODIFICACION-99. EXIT.                  

       400-CONSULTA-00.
           DISPLAY " " ERASE
           MOVE "CONSULTA DE PROVINCIAS" TO W-TITULO-ENC
           PERFORM ENCABEZADO THRU ENCABEZADO-F.
           DISPLAY "INGRESE CANTIDAD REGISTROS:" LINE 5 POSITION 1.
           ACCEPT CANTPEDIDA LINE 5 POSITION 30 PROMPT.
           CLOSE PROVINCIAS        
           OPEN INPUT PROVINCIAS
           MOVE 6 TO UBICACIONFILA.
       400-CONSULTA-10.
           READ PROVINCIAS NEXT AT END
                GO TO 400-CONSULTA-90
           END-READ

           IF FILA = 0
             DISPLAY " " ERASE
             MOVE "CONSULTA DE PROVINCIAS" TO W-TITULO-ENC
             PERFORM ENCABEZADO THRU ENCABEZADO-F
             PERFORM COLUMNAS THRU COLUMNAS-F
           END-IF.

           COMPUTE UBICACIONFILA = UBICACIONFILA + 1
           COMPUTE FILA = FILA + 1
		   
           MOVE PRO-CODIGO      TO D1-CODIGO   
           MOVE PRO-DESCRIPCION TO D1-DESCRIPCION   
           MOVE PRO-ABREVIATURA TO D1-ABREVIATURA
		   
           DISPLAY DETALLE-01 LINE UBICACIONFILA POSITION 1.

           IF FILA = CANTPEDIDA
              MOVE 0 TO FILA
              MOVE 6 TO UBICACIONFILA
              ACCEPT SINO LINE 24 POSITION 1
           END-IF.
           GO TO 400-CONSULTA-10.
       400-CONSULTA-90.
           CLOSE PROVINCIAS.
           OPEN I-O PROVINCIAS.
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

       PANTALLAINICIO.
           DISPLAY "1-ALTA         "   LINE 5  POSITION 1.
           DISPLAY "2-BAJA         "   LINE 6  POSITION 1.
           DISPLAY "3-MODIFICACION "   LINE 7  POSITION 1.
           DISPLAY "4-CONSULTA     "   LINE 8  POSITION 1.
           DISPLAY "9-SALIR        "   LINE 9  POSITION 1.
           DISPLAY "OPCION:"           LINE 12 POSITION 1.
       PANTALLAINICIO-F. EXIT.
	   
       PANTALLA.
           DISPLAY "CODIGO    :"       LINE 5 POSITION 1.
           DISPLAY "DESC.     :"       LINE 6 POSITION 1.
           DISPLAY "ABREV.    :"       LINE 7 POSITION 1.
           DISPLAY "CONFIRMA? (S/N): " LINE 10 POSITION 1.
       PANTALLA-F. EXIT.

       ERRORES.
           DISPLAY M-ERROR LINE 24 POSITION 1.
           ACCEPT SINO LINE 24 POSITION 80.
           INITIALIZE M-ERROR.
           DISPLAY M-ERROR LINE 24 POSITION 1.
       ERRORES-F. EXIT.

