       IDENTIFICATION DIVISION.
       PROGRAM-ID. Productos AS "Productos".
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Productos ASSIGN TO 'Productos.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS P-ID
               FILE STATUS IS WS-FileStatus.

       DATA DIVISION.
       FILE SECTION.
       FD  Productos.
       01  Product.
           05 P-ID                    PIC X(10).
           05 P-Nombre                PIC X(30).
           05 P-Stock                 PIC 9(7).
           05 P-Precio-Unitario       PIC 9(5)V99.
           05 P-Categoria             PIC X(20).
           05 P-Proveedor             PIC X(50).
           05 P-Fecha-Registro.
               10 Registro-Ano     PIC 9(4).
               10 Registro-Mes     PIC 9(2).
               10 Registro-Dia     PIC 99.
           05 P-Fecha-Modificacion.
               10 Modificacion-Ano PIC 9(4).
               10 Modificacion-Mes PIC 9(2).
               10 Modificacion-Dia PIC 99.
           05 P-Ubicacion             PIC X(50).
           05 P-Stock-Minimo          PIC 9(4).
           05 P-Estado                PIC X(10).
           05 P-Descripcion           PIC X(100).
           05 P-Unidad-Medida         PIC X(2).

       WORKING-STORAGE SECTION.
           01 WS-Control.
               05 WS-FileStatus        PIC XX.
               05 WS-EOF-Flag  PIC X(1) VALUE "N".

           01 WS-Rankings.
               05 WS-Maximo    PIC 9(7) VALUE 0.
               05 WS-Minimo    PIC 9(7) VALUE 9999999.

       LINKAGE SECTION.
           01 LK-Flag    PIC 9(2).
           01 LK-NombreP PIC X(30).

       PROCEDURE DIVISION USING LK-Flag, LK-NombreP.
       MAIN-PROCEDURE.
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
                               IF  P-Stock  > WS-Maximo THEN
                                   MOVE P-Nombre  TO LK-NombreP
                                   MOVE P-Stock TO WS-Maximo
                               END-IF
                           WHEN 4
                               IF  P-Stock  < WS-Minimo THEN
                                   MOVE P-Nombre TO LK-NombreP
                                   MOVE P-Stock TO WS-Minimo
                               END-IF
                           WHEN 5
                               IF  P-Stock  < P-Stock-Minimo THEN
                                   DISPLAY P-Nombre "Stock minimo: "
                                   P-Stock-Minimo " Actual: "P-Stock
                               END-IF
                       END-EVALUATE
                END-READ
           END-PERFORM
           CLOSE Productos
           MOVE 'N'     TO WS-EOF-Flag
           MOVE 9999999 TO WS-Minimo
           MOVE 0       TO WS-Maximo
           EXIT.
