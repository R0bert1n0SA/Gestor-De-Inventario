       IDENTIFICATION DIVISION.
       PROGRAM-ID. Productos AS "Productos".
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
           01 WS-Parametros.
               05 LK-Flag          PIC 9(2).
               05 LK-Maximo        PIC 9(7).
               05 LK-Minimo        PIC 9(7).
               05 LK-NombreFinal   PIC X(30).
               05 LK-Nombre        PIC X(30).
               05 LK-Stock         PIC 9(7).
           01 LK-Stock-Minimo  PIC 9(7).


       PROCEDURE DIVISION USING WS-Parametros,LK-Stock-Minimo.
       MAIN-PROCEDURE.
           PERFORM Ranking
       EXIT PROGRAM.


       Rankingg SECTION.
           Ranking.
               EVALUATE LK-Flag
                   WHEN 3
                       IF  LK-Stock > LK-Maximo THEN
                           MOVE LK-Nombre TO LK-NombreFinal
                           MOVE LK-Stock TO LK-Maximo
                       END-IF
                   WHEN 4
                       IF  LK-Stock < LK-Minimo THEN
                           MOVE LK-Nombre TO LK-NombreFinal
                           MOVE LK-Stock TO LK-Minimo
                       END-IF
                   WHEN 5
                       IF  LK-Stock  < LK-Stock-Minimo THEN
                           DISPLAY LK-Nombre "Stock minimo: "
                           LK-Stock-Minimo " Actual: "LK-Stock
                       END-IF
               END-EVALUATE
           EXIT.
