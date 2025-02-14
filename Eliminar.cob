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
               PERFORM Inicio
           EXIT PROGRAM.
      *================================================================*
       *> SECCION ELiminar
       *> Solicita el ID del producto y procede a eliminarlo.
      *================================================================*
       Eliminar SECTION.
           Inicio.
               DISPLAY "Ingrese el id a eliminar: "
               ACCEPT WS-PID
               OPEN I-O Productos
               IF WS-File-Status = '00' THEN
                   PERFORM Buscar
                   CLOSE Productos
               ELSE
                   CALL "Errores" USING WS-File-Status
               END-IF.
           EXIT.

           Buscar.
               MOVE WS-PID TO Product-ID
               READ Productos INTO Product KEY IS Product-ID
                   NOT INVALID KEY
                       DELETE Productos
                       DISPLAY "Producto eliminado exitosamente."
               END-READ.
           EXIT.
      *================================================================*
