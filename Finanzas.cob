       IDENTIFICATION DIVISION.
       PROGRAM-ID. Finanzas AS "Finanzas".
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
           01 WS-Ps       PIC XX.
           01 WS-EOF-Flag PIC X(1) VALUE "N".
           01 WS-Total    PIC 9(9)v99.
           01 WS-Top      PIC 9(5)v99.
           01 WS-Estado  PIC 9(1).
           01 WS-name     PIC X(30).
       LINKAGE SECTION.
           01 LK-Flag     PIC 9(2).
           01 LK-name     PIC X(30).
           01 LK-Total    PIC 9(9)v99.
           01 LK-Top      PIC 9(5)v99.

       PROCEDURE DIVISION USING LK-Flag,LK-name,LK-Total,LK-Top.
       MAIN-PROCEDURE.
           MOVE LK-Top TO WS-Top
           PERFORM Finanzass
           IF WS-Estado = 1 THEN
               MOVE WS-Total TO LK-Total
               MOVE " " TO LK-name
               MOVE 0  TO LK-Top
           ELSE
               MOVE 0 TO LK-Total
               MOVE WS-name TO LK-name
               MOVE WS-Top  TO LK-Top
           END-IF
       EXIT PROGRAM.



       Finanzass.
           OPEN INPUT Productos
           MOVE 2 TO WS-Estado
           PERFORM UNTIL WS-EOF-Flag = 'Y'
               READ Productos INTO Product
                   AT END
                       MOVE 'Y' TO WS-EOF-Flag
                       DISPLAY "Fin de archivo alcanzado"
                   NOT AT END
                       EVALUATE LK-Flag
                           WHEN 8
                               IF WS-Estado = 2 THEN
                                   MOVE 1 TO WS-Estado
                               END-IF
                               COMPUTE WS-Total=(WS-Total +
                               (Stock * Precio-Unitario))

                           WHEN 9
                               IF Precio-Unitario > WS-Top THEN
                                   MOVE Precio-Unitario TO WS-Top
                                   MOVE Nombre TO WS-name
                               END-IF
                           WHEN 10
                               IF Precio-Unitario < WS-Top THEN
                                   MOVE Precio-Unitario TO WS-Top
                                   MOVE Nombre   TO WS-name
                               END-IF
                       END-EVALUATE
               END-READ
           END-PERFORM
           CLOSE Productos
           MOVE 'N' TO WS-EOF-Flag
           EXIT.
