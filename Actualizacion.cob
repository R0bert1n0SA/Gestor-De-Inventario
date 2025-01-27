      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
           PROGRAM-ID. Actualizacion AS "Actualizar".
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
           01 Productos-status PIC XX.
           01 Fecha            PIC 9(8).
           01 Year             PIC 9(4)  VALUE 2000.
           01 PID              PIC 9(10).
           PROCEDURE DIVISION.
           MAIN-PROCEDURE.
               PERFORM Leer
           EXIT PROGRAM.



       Leer.
           DISPLAY "Ingrese el id: "
           ACCEPT PID
           OPEN I-O Productos
           MOVE PID TO Product-ID
           READ Productos INTO Product KEY IS Product-ID
           INVALID KEY
               DISPLAY "No existe el producto"
               CLOSE Productos
               GOBACK
           NOT INVALID KEY
               PERFORM ACTUALIZAR
               REWRITE Product
               DISPLAY "Actualizado correctamente"
               CLOSE Productos
               GOBACK
           END-READ
           EXIT.



       ACTUALIZAR.
           DISPLAY "Ingrese Stock Actual actualizar: "
           ACCEPT   Stock
           DISPLAY "Ingrese Precio: "
           ACCEPT   Precio-Unitario
           ACCEPT  Fecha FROM DATE
           MOVE    Fecha(7:2) TO Dia-Modificacion
           MOVE    Fecha(5:2) TO Mes-Modificacion
           MOVE    Fecha(1:4) TO Ano-Modificacion
           ADD Ano-Modificacion TO Year GIVING Ano-Modificacion
           DISPLAY  "Ingrese ubicacion: "
           ACCEPT   Ubicacion
           DISPLAY  "Ingrese Estado: "
           ACCEPT   Estado
           EXIT.
