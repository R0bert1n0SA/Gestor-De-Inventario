       IDENTIFICATION DIVISION.
       PROGRAM-ID. Productos AS "Productos".
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Productos ASSIGN TO 'Productos.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS Product-ID
               FILE STATUS IS WS-Ps.

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
           01 WS-Ps        PIC XX.
           01 WS-EOF-Flag  PIC X(1) VALUE "N".
           01 WS-Maximo    PIC 9(7) VALUE 0.
           01 WS-Minimo    PIC 9(7) VALUE 9999999.

       LINKAGE SECTION.
           01 LK-Flag    PIC 9(2).
           01 LK-NombreP PIC X(30).

       PROCEDURE DIVISION USING LK-Flag, LK-NombreP.
       MAIN-PROCEDURE.
           IF LK-Flag = 5 THEN
               DISPLAY "---------Productos con Bajo Stock------------"
           END-IF
           PERFORM Ranking
       EXIT PROGRAM.



       Ranking.
           OPEN INPUT Productos
           PERFORM UNTIL WS-EOF-Flag = 'Y'
               READ Productos INTO Product
                   AT END
                       MOVE 'Y' TO WS-EOF-Flag
                   NOT AT END
                       EVALUATE LK-Flag
                           WHEN 3
                               IF Stock > WS-Maximo THEN
                                   MOVE Nombre TO LK-NombreP
                               END-IF
                           WHEN 4
                               IF Stock > WS-Minimo THEN
                                   MOVE Nombre TO LK-NombreP
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
           MOVE 'N' TO WS-EOF-Flag
           EXIT.
