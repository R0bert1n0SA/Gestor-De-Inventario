       IDENTIFICATION DIVISION.
           PROGRAM-ID. Mostrar AS "Mostrar".
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
               05 Product-ID            PIC X(10).
               05 Nombre               PIC X(30).
               05 Cantidad             PIC S9(7).
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
               05 Stock-Minimo         PIC 9(4).
               05 Estado               PIC X(10).
               05 Descripcion          PIC X(100).
               05 Unidad-Medida        PIC X(2).

           WORKING-STORAGE SECTION.
           01  Productos-status        PIC XX.
           01  EOF-Flag          PIC X(1).

           PROCEDURE DIVISION.
           MAIN-PROCEDURE.
               PERFORM MOSTRAR
           EXIT PROGRAM.


       MOSTRAR.
           MOVE 'N' TO EOF-Flag
           OPEN INPUT Productos
           PERFORM UNTIL EOF-Flag = 'Y'
               READ Productos INTO Product
                   AT END
                       MOVE 'Y' TO EOF-Flag
                       DISPLAY "Fin de archivo alcanzado"
                   NOT AT END
                       DISPLAY "--------------------------------"
                       DISPLAY "ID: " Product-ID
                       DISPLAY "Nombre: " Nombre
                       DISPLAY "Cantidad: " Cantidad
                       DISPLAY "Precio Unitario: " Precio-Unitario
                       DISPLAY "Categoria: " Categoria
                       DISPLAY "Proveedor: " Proveedor
                       DISPLAY "Fecha Registro: "
                           Dia-Registro "/" Mes-Registro
                           "/" Ano-Registro
                       DISPLAY "Fecha Modificacion: "
                           Dia-Modificacion "/" Mes-Modificacion "/"
                           Ano-Modificacion
                       DISPLAY "Ubicacion: " Ubicacion
                       DISPLAY "Stock Minimo: " Stock-Minimo
                       DISPLAY "Estado: " Estado
                       DISPLAY "Descripcion: " Descripcion
                       DISPLAY "Unidad de Medida: " Unidad-Medida
               END-READ
           END-PERFORM
           CLOSE Productos
           EXIT.
