       IDENTIFICATION DIVISION.
       PROGRAM-ID.General AS "General".
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
           01 LK-Flag        PIC 9(2).
           01 LK-Parametros.
               05 Contador    PIC 9(9).
           01 LK-Stock       PIC 9(7).

       PROCEDURE DIVISION USING LK-Flag,LK-Parametros,LK-Stock.
       MAIN-PROCEDURE.
           PERFORM Evaluando
       EXIT PROGRAM.

       Evaluando.
           EVALUATE LK-Flag
               WHEN 1
                   COMPUTE Contador=(Contador + 1)
               WHEN 2
                   COMPUTE Contador=(Contador + LK-Stock)
           END-EVALUATE
       EXIT.
