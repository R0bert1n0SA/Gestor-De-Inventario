       IDENTIFICATION DIVISION.
       PROGRAM-ID.General AS "General".
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Productos ASSIGN TO 'Productos.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS Product-ID
               FILE STATUS IS WS-FileStatus.

       DATA DIVISION.
       FILE SECTION.
       FD  Productos.
       01  Product.
           05 Product-ID     PIC X(10).
           05 Stock          PIC 9(7).

       WORKING-STORAGE SECTION.
           01 WS-FileStatus  PIC XX.
           01 WS-EOF-Flag    PIC X(1) VALUE "N".
       LINKAGE SECTION.
           01 LK-Flag        PIC 9(2).
           01 LK-Contador    PIC 9(9) VALUE 0.

       PROCEDURE DIVISION USING LK-Flag,LK-Contador.
       MAIN-PROCEDURE.
           PERFORM Contando
       EXIT PROGRAM.

      *================================================================*
       *> SECCIÓN Contar
       *>  Intenta abrir el archivo y, si tiene éxito,procede a recorrer
       *>  los registros.
      *================================================================*
       Contar SECTION.
           Contando.
               OPEN INPUT Productos
               IF WS-FileStatus NOT = '00' THEN
                   PERFORM Recorrer
               ELSE
                   CALL "Errores" USING WS-FileStatus
               END-IF
           EXIT.

           Recorrer.
               PERFORM UNTIL WS-EOF-Flag = 'Y'
                   READ Productos INTO Product
                       AT END
                           MOVE 'Y' TO WS-EOF-Flag
                       NOT AT END
                           PERFORM Evaluando
                   END-READ
               END-PERFORM
               CLOSE Productos
               MOVE 'N' TO WS-EOF-Flag
           EXIT.


           Evaluando.
               EVALUATE LK-Flag
                   WHEN 1
                       COMPUTE LK-Contador=(LK-Contador + 1)
                   WHEN 2
                       COMPUTE LK-Contador=(LK-Contador + Stock)
               END-EVALUATE
           EXIT.
      *================================================================*
