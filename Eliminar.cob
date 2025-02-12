       IDENTIFICATION DIVISION.
       PROGRAM-ID. Eliminar AS "Eliminar".

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Productos ASSIGN TO 'Productos.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS Product-ID
               FILE STATUS IS WS-File-Status.

       DATA DIVISION.
       FILE SECTION.
       FD  Productos.
       01  Product.
           05 Product-ID           PIC X(10).

       WORKING-STORAGE SECTION.
       01  WS-File-Status        PIC XX.
       01  WS-PID                     PIC X(10).

       PROCEDURE DIVISION.
           MAIN-PROCEDURE.
               PERFORM Buscar-Dato
           EXIT PROGRAM.

       Buscar-Dato.
           DISPLAY "Ingrese el id a eliminar : "
           ACCEPT WS-PID
           OPEN I-O Productos
           IF WS-File-Status  = '00' THEN
               PERFORM Recorrer-Archivo
               CLOSE Productos
           ELSE
               DISPLAY "Error : "WS-File-Status
           END-IF
       EXIT.


       Recorrer-Archivo.
           MOVE WS-PID TO Product-ID
           READ Productos INTO Product KEY IS Product-ID
               NOT INVALID KEY
                   DELETE Productos
           END-READ
       EXIT.
