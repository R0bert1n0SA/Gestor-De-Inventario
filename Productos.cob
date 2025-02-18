       IDENTIFICATION DIVISION.
       PROGRAM-ID. Productos AS "Productos".
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
           01 LK-Flag          PIC 9(2).
           01 LK-Stock         PIC 9(7).
           01 LK-Nombre        PIC X(30).
           01 LK-Parametros.
               05 LK-top           PIC 9(7).
               05 LK-NombreFinal   PIC X(30).
           01 LK-Stock-Minimo  PIC 9(7).


       PROCEDURE DIVISION USING LK-Flag,LK-Stock,LK-Nombre
       ,LK-Parametros,LK-Stock-Minimo.
       MAIN-PROCEDURE.
           PERFORM Ranking
       EXIT PROGRAM.


       Rankingg SECTION.
           Ranking.
               EVALUATE LK-Flag
                   WHEN 3
                       IF  LK-Stock > LK-top THEN
                           MOVE LK-Nombre TO LK-NombreFinal
                           MOVE LK-Stock TO LK-top
                       END-IF
                   WHEN 4
                       IF  LK-Stock < LK-top THEN
                           MOVE LK-Nombre TO LK-NombreFinal
                           MOVE LK-Stock TO LK-top
                       END-IF
                   WHEN 5
                       IF  LK-Stock  < LK-Stock-Minimo THEN
                           DISPLAY LK-Nombre "Stock minimo: "
                           LK-Stock-Minimo " Actual: "LK-Stock
                       END-IF
               END-EVALUATE
           EXIT.
