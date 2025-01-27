       IDENTIFICATION DIVISION.
       PROGRAM-ID. Carga AS "Carga".

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
       01  Productos-status        PIC XX.
       01  PID                     PIC X(10).
       01  opcion                  PIC X(1).
       01  Fecha                   PIC 9(8).
       01  EOF-Flag          PIC X(1).


       PROCEDURE DIVISION.
           MAIN-PROCEDURE.
               PERFORM CARGAR-DATOS
           EXIT PROGRAM.



       INGRESO.
           DISPLAY "Ingrese nombre del producto: "
           ACCEPT   Nombre
           DISPLAY "Ingrese Stock Actual del producto: "
           ACCEPT   Stock
           DISPLAY "Ingrese Precio: "
           ACCEPT   Precio-Unitario
           DISPLAY "Ingrese categoria del Producto: "
           ACCEPT   Categoria
           DISPLAY "Ingrese Proveedor: "
           ACCEPT   Proveedor
           ACCEPT   Fecha FROM DATE
           MOVE     Fecha(7:2) TO Dia-Registro
           MOVE     Fecha(5:2) TO Mes-Registro
           MOVE     Fecha(1:4) TO Ano-Registro
           DISPLAY  "Ingrese ubicacion: "
           ACCEPT   Ubicacion
           DISPLAY "Ingrese Stock minimo: "
           ACCEPT   Stock-Minimo
           DISPLAY  "Ingrese Estado: "
           ACCEPT   Estado
           DISPLAY "Ingrese descripcion: "
           ACCEPT   Descripcion
           DISPLAY "Ingrese unidad de medida"
           ACCEPT  Unidad-Medida
           EXIT.

       CARGAR-DATOS.
           DISPLAY "Ingrese el id: "
           ACCEPT PID
           OPEN I-O Productos
           PERFORM BUSCAR-DATO
           CLOSE Productos
           EXIT.

       BUSCAR-DATO.
           MOVE PID TO Product-ID
           READ Productos INTO Product KEY IS Product-ID
           INVALID KEY
               PERFORM INGRESO
               WRITE Product
               DISPLAY X"1B" & "[2J"
               DISPLAY "Registro guardado correctamente"
           NOT INVALID KEY
               EXIT
           END-READ
           EXIT.
