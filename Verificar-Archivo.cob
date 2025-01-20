       IDENTIFICATION DIVISION.
           PROGRAM-ID. Verificar-Archivo AS "Verificar-Archivo".
           ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT Productos ASSIGN TO 'Productos.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS Product-ID
               FILE STATUS IS Productos-status.

           DATA DIVISION.
           FILE SECTION.
           FD  Productos.
           01 Product-ID PIC XX.
           WORKING-STORAGE SECTION.
           01 Productos-status PIC XX.

           PROCEDURE DIVISION.
           MAIN-PROCEDURE.
               PERFORM Existe
           EXIT PROGRAM.

       Existe.
           OPEN INPUT Productos
           IF Productos-status = "35" THEN
              OPEN OUTPUT Productos
              CLOSE Productos
           ELSE
              CLOSE Productos
           END-IF
           EXIT.
