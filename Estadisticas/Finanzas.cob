       IDENTIFICATION DIVISION.
       PROGRAM-ID. Finanzas AS "Finanzas".
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Productos
           ASSIGN TO
           'F:\Proyectos\Cobol\Gestion de Inventarios\bin\Productos.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS Product-ID
               FILE STATUS IS Ps.
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
           01 Ps       PIC XX.
           01 EOF-Flag PIC X(1) VALUE "N".
           01 Total    PIC 9(9)v99.
           01 minimo   PIC 9(5)v99 VALUE 0.
           01 maximo   PIC 9(5)v99 VALUE 99999.
           01 name     PIC X(30).
       LINKAGE SECTION.
           01 Flag PIC 9(2).
       PROCEDURE DIVISION USING Flag.
       MAIN-PROCEDURE.
           PERFORM Finanzass
       EXIT PROGRAM.



       Finanzass.
           OPEN INPUT Productos
           PERFORM UNTIL EOF-Flag = 'Y'
               READ Productos INTO Product
                   AT END
                       MOVE 'Y' TO EOF-Flag
                       DISPLAY "Fin de archivo alcanzado"
                   NOT AT END
                       EVALUATE Flag
                           WHEN 8
                               COMPUTE Total=(Total +
                               (Stock * Precio-Unitario))
                           WHEN 9
                               IF Precio-Unitario > maximo THEN
                                   MOVE Precio-Unitario TO maximo
                                   MOVE Nombre TO name
                               END-IF
                           WHEN 10
                               IF Precio-Unitario < Minimo THEN
                                   MOVE Precio-Unitario TO minimo
                                   MOVE Nombre   TO name
                               END-IF
                       END-EVALUATE
               END-READ
           END-PERFORM
           CLOSE Productos
           IF Flag = 8  THEN
               DISPLAY "Costo Total de Inventario: "Total
           END-IF
           IF Flag = 9  THEN
               DISPLAY "EL Producto Mas Caro es: " name
               " Precio: "maximo
           END-IF
           IF Flag = 10 THEN
               DISPLAY "EL Producto Mas Barato es: " name
               " Precio: "maximo
           END-IF
           EXIT.
