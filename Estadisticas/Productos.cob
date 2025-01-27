       IDENTIFICATION DIVISION.
       PROGRAM-ID. Productos AS "Productos".
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Productos ASSIGN TO 'Productos.DAT'
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
           01 Maximo   PIC 9(7) VALUE 0.
           01 Minimo   PIC 9(7) VALUE 9999999.

       LINKAGE SECTION.
           01  Flag  PIC 9(2).
           01  NombreP PIC X(30).

       PROCEDURE DIVISION USING Flag, NombreP.
       MAIN-PROCEDURE.
           IF Flag = 5 THEN
               DISPLAY "---------Productos con Bajo Stock------------"
           END-IF
           PERFORM Ranking
       EXIT PROGRAM.



       Ranking.
           OPEN INPUT Productos
           PERFORM UNTIL EOF-Flag = 'Y'
               READ Productos INTO Product
                   AT END
                       MOVE 'Y' TO EOF-Flag
                   NOT AT END
                       EVALUATE Flag
                           WHEN 3
                               IF Stock > Maximo THEN
                                   MOVE Nombre TO NombreP
                               END-IF
                           WHEN 4
                               IF Stock > Minimo THEN
                                   MOVE Nombre TO NombreP
                               END-IF
                           WHEN 5
                               IF Stock > Stock-Minimo THEN
                                   DISPLAY Nombre " Stock minimo: "
                                   Stock-Minimo " Actual: " Stock
                                   Stock-Minimo
                               END-IF
                       END-EVALUATE
                END-READ
           END-PERFORM
           CLOSE Productos
           EXIT.
