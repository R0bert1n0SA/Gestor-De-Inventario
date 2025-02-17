       IDENTIFICATION DIVISION.
       PROGRAM-ID. Finanzas AS "Finanzas".
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
           01 LK-Parametros.
               05 Flag             PIC 9(2).
               05 Total            PIC 9(9)v99.
               05 Nombre           PIC X(30).
               05 NombreF          PIC X(30).
               05 Precio-Unitario  PIC 9(5)V99.
               05 Stock            PIC 9(7).
               05 Tope             PIC 9(5)v99.
       PROCEDURE DIVISION USING LK-Parametros.
       MAIN-PROCEDURE.

           PERFORM Evaluar
       EXIT PROGRAM.

           Evaluar.
               EVALUATE Flag
                   WHEN 8
                       COMPUTE Total=(Total +
                       (Stock * Precio-Unitario))
                   WHEN 9
                       IF Precio-Unitario > Tope THEN
                           MOVE Precio-Unitario TO Tope
                           MOVE Nombre TO NombreF
                       END-IF
                   WHEN 10
                       IF Precio-Unitario < Tope THEN
                           MOVE Precio-Unitario TO Tope
                           MOVE Nombre   TO NombreF
                       END-IF
               END-EVALUATE
           EXIT.
