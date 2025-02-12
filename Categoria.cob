       IDENTIFICATION DIVISION.
       PROGRAM-ID. Categoria AS "Categoria".
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Productos ASSIGN TO 'Productos.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS Product-ID
               FILE STATUS IS WS-Ps.


           SELECT TCont ASSIGN TO 'Temporal-Cont'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TC-Categoria
               FILE STATUS IS WS-Ps2.

       DATA DIVISION.
       FILE SECTION.
       FD  Productos.
       01  Product.
           05 Product-ID           PIC X(10).
           05 Categoria            PIC X(20).

       FD TCont.
       01 Contador.
           05 TC-Categoria         PIC X(20).
           05 TC-Total             PIC 9(9).

       WORKING-STORAGE SECTION.
           01 WS-Control.
               05 WS-Ps           PIC XX.
               05 WS-Ps2          PIC XX.
               05 WS-EOF-Flag     PIC X(1) VALUE "N".
               05 WS-EOF-FlagCa   PIC X(1) VALUE 'N'.

           01 WS-maximo       PIC 9(9) VALUE 0.
           01 WS-cant         PIC 9(9) VALUE 0.
           01 WS-CateMax      PIC X(20) VALUE " ".
           01 WS-CateAct      PIC X(20) VALUE SPACES.

       LINKAGE SECTION.
           01 LK-Flag   PIC 9(2).

       PROCEDURE DIVISION USING LK-Flag.
       MAIN-PROCEDURE.
           PERFORM Categoria-op
           IF LK-Flag = 7 THEN
               DISPLAY WS-CateMax ": "WS-maximo
           END-IF
       EXIT PROGRAM.



       CrearTC.
           OPEN INPUT TCont
           IF WS-Ps2 = "35" THEN
              OPEN OUTPUT TCont
              CLOSE TCont
           ELSE
              CLOSE TCont
           END-IF
           EXIT.


       Mostrar.
           OPEN INPUT TCont
           PERFORM UNTIL WS-EOF-FlagCa = 'Y'
               READ TCont INTO Contador
                   AT END
                       MOVE 'Y' TO WS-EOF-FlagCa
                   NOT AT END
                       EVALUATE LK-Flag
                       WHEN 6
                           DISPLAY TC-Categoria ": "TC-Total
                       WHEN 7
                           IF TC-Total > WS-maximo THEN
                               MOVE TC-Total TO WS-maximo
                               MOVE TC-Categoria TO WS-CateMax
                           END-IF
                       END-EVALUATE
               END-READ
           END-PERFORM
           MOVE 'N' TO WS-EOF-FlagCa
           CLOSE TCont
           EXIT.


       Contabilizar.
           OPEN I-O TCont
           READ TCont INTO Contador KEY IS TC-Categoria
               INVALID KEY
                   MOVE 1 TO TC-Total
                   WRITE Contador
                   MOVE SPACES TO TC-Categoria
               NOT INVALID KEY
                   ADD 1 TO TC-Total GIVING TC-Total
                   REWRITE Contador
                   MOVE SPACES TO TC-Categoria
           END-READ
           CLOSE TCont
           EXIT.


       Categoria-op.
           OPEN INPUT Productos
           PERFORM CrearTC
           PERFORM Until WS-EOF-Flag = 'Y'
               READ Productos INTO Product
                   AT END
                       MOVE 'Y' TO WS-EOF-Flag
                   NOT AT END
                       MOVE Categoria TO TC-Categoria
                       PERFORM Contabilizar
               END-READ
           END-PERFORM
           MOVE 'N' TO WS-EOF-Flag
           PERFORM Mostrar
           DELETE FILE TCont
           Close Productos
           EXIT.
