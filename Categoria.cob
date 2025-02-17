       IDENTIFICATION DIVISION.
       PROGRAM-ID. Categoria AS "Categoria".
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT TCont ASSIGN TO 'Temporal-Cont'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TC-Categoria
               FILE STATUS IS WS-File-StatusTemp .

       DATA DIVISION.
       FILE SECTION.
       FD TCont.
       01 Contador.
           05 TC-Categoria         PIC X(20).
           05 TC-Total             PIC 9(9).

       WORKING-STORAGE SECTION.
           01 WS-Control.
               05 WS-File-StatusTemp   PIC XX.
               05 WS-EOF-FlagCa        PIC X(1) VALUE 'N'.

       LINKAGE SECTION.
           01 LK-Parametros.
               05 Flag   PIC 9(2).
               05 Categoria PIC X(20).

       PROCEDURE DIVISION USING LK-Parametros.
       MAIN-PROCEDURE.
           PERFORM Categoria-op
       EXIT PROGRAM.



       Contabilizar.
           OPEN I-O TCont
           READ TCont INTO Contador KEY IS TC-Categoria
               INVALID KEY
                   MOVE 1 TO TC-Total
                   WRITE Contador
                   MOVE SPACES TO Categoria
               NOT INVALID KEY
                   ADD 1 TO TC-Total GIVING TC-Total
                   REWRITE Contador
                   MOVE SPACES TO TC-Categoria
           END-READ
           CLOSE TCont
           EXIT.


       Categoria-op.
           MOVE FUNCTION TRIM(Categoria) TO TC-Categoria
           PERFORM Contabilizar
       EXIT.
