       IDENTIFICATION DIVISION.
       PROGRAM-ID. Eliminar AS "Eliminar".

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Productos ASSIGN TO 'Productos.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS Product-ID
               FILE STATUS IS WS-Productos-status.

       DATA DIVISION.
       FILE SECTION.
       FD  Productos.
       01  Product.
           05 Product-ID           PIC X(10).
           05 Nombre               PIC X(30).
           05 Stock                PIC 9(7).
           05 Precio-Unitario      PIC 9(5)V99.
           05 Categoria            PIC X(20).
           05 Proveedor            PIC X(50).
           05 Fecha-Registro.
               10 Ano-Registro     PIC 9(4).
               10 Mes-Registro     PIC 9(2).
               10 Dia-Registro     PIC 99.
           05 Fecha-Modificacion.
               10 Ano-Modificacion PIC 9(4).
               10 Mes-Modificacion PIC 9(2).
               10 Dia-Modificacion PIC 99.
           05 Ubicacion            PIC X(50).
           05 Stock-Minimo         PIC 9(7).
           05 Estado               PIC X(10).
           05 Descripcion          PIC X(100).
           05 Unidad-Medida        PIC X(2).

       WORKING-STORAGE SECTION.
       01  WS-Productos-status        PIC XX.
       01  WS-PID                     PIC X(10).

       PROCEDURE DIVISION.
           MAIN-PROCEDURE.
               PERFORM BUSCAR-DATO
           EXIT PROGRAM.


       ELIMINAR.
           DELETE Productos
               INVALID KEY
                    DISPLAY "Error al eliminar el producto."
                NOT INVALID KEY
                    DISPLAY "Producto eliminado exitosamente."
           END-DELETE
           EXIT.

       BUSCAR-DATO.
           DISPLAY "Ingrese el id: "
           ACCEPT WS-PID
           OPEN I-O Productos
           MOVE WS-PID TO Product-ID
           READ Productos INTO Product KEY IS Product-ID
           INVALID KEY
               DISPLAY "Producto no existe"
               CLOSE Productos
               GOBACK
           NOT INVALID KEY
               PERFORM Eliminar
               CLOSE Productos
               GOBACK
           END-READ
           EXIT.
