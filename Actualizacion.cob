       IDENTIFICATION DIVISION.
           PROGRAM-ID. Actualizacion AS "Actualizar".
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
               05 Stock                PIC 9(7).
               05 Precio-Unitario      PIC 9(5)V99.
               05 Fecha-Modificacion.
                   10 Ano-Modificacion PIC 9(4).
                   10 Mes-Modificacion PIC 9(2).
                   10 Dia-Modificacion PIC 99.
               05 Ubicacion            PIC X(50).
               05 Estado               PIC X(10).
               05 Descripcion          PIC X(100).

           WORKING-STORAGE SECTION.
           01 WS-File-Status PIC XX.
           01 WS-Fecha            PIC 9(8).
           01 WS-Year             PIC 9(4)  VALUE 2000.
           01 WS-PID              PIC 9(10).
           PROCEDURE DIVISION.
           MAIN-PROCEDURE.
               PERFORM Leer
           EXIT PROGRAM.



      *============================================================*
      *> SECCION Leer-Producto
      *> Solicita el ID del producto y verifica si existe
      *============================================================*
       Iniciar SECTION.
           Leer.
               DISPLAY "Ingrese el id: "
               ACCEPT WS-PID
               OPEN I-O Productos
               IF WS-File-Status = '00' THEN
                   PERFORM Recorrer
               ELSE
                   CALL "Errores" USING WS-File-Status
               END-IF

           EXIT.


           Recorrer.
               MOVE WS-PID TO Product-ID
               READ Productos INTO Product KEY IS Product-ID
               INVALID KEY
                   DISPLAY "No existe el producto"
                   CLOSE Productos
                   GOBACK
               NOT INVALID KEY
                   PERFORM Actualizar
                   REWRITE Product
                   DISPLAY "Actualizado correctamente"
                   CLOSE Productos
                   GOBACK
               END-READ
           EXIT.

           Actualizar.
               DISPLAY "Ingrese Stock Actual actualizar: "
               ACCEPT   Stock
               DISPLAY "Ingrese Precio: "
               ACCEPT   Precio-Unitario
               ACCEPT  WS-Fecha FROM DATE
               MOVE    WS-Fecha(7:2) TO Dia-Modificacion
               MOVE    WS-Fecha(5:2) TO Mes-Modificacion
               MOVE    WS-Fecha(1:4) TO Ano-Modificacion
               ADD Ano-Modificacion TO WS-Year GIVING Ano-Modificacion
               DISPLAY  "Ingrese ubicacion: "
               ACCEPT   Ubicacion
               DISPLAY  "Ingrese Estado: "
               ACCEPT   Estado
           EXIT.
      *============================================================*
