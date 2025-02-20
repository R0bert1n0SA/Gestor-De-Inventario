       IDENTIFICATION DIVISION.
       PROGRAM-ID. Verificar-Archivo AS "Verificar-Archivo".
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
           01 Product.
               05 Product-ID PIC X(10).

       WORKING-STORAGE SECTION.
           01 WS-FileStatus PIC XX.

       PROCEDURE DIVISION .
           MAIN-PROCEDURE.
               PERFORM Existe
           EXIT PROGRAM.


       Existe.
           OPEN INPUT Productos
           IF WS-FileStatus = "35" THEN
              OPEN OUTPUT Productos
              CLOSE Productos
           ELSE
              CLOSE Productos
           END-IF
           EXIT.
