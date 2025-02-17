       IDENTIFICATION DIVISION.
       PROGRAM-ID.General AS "General".
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
           01 LK-Parametros.
               05 LK-Flag        PIC 9(2).
               05 LK-Contador    PIC 9(9).
               05 LK-Stock       PIC 9(7).

       PROCEDURE DIVISION USING LK-Parametros.
       MAIN-PROCEDURE.
           PERFORM Evaluando
       EXIT PROGRAM.

       Evaluando.
           EVALUATE LK-Flag
               WHEN 1
                   COMPUTE LK-Contador=(LK-Contador + 1)
               WHEN 2
                   COMPUTE LK-Contador=(LK-Contador + LK-Stock)
           END-EVALUATE
       EXIT.
