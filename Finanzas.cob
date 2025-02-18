       IDENTIFICATION DIVISION.
       PROGRAM-ID. Finanzas AS "Finanzas".
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
           01 LK-Flag             PIC 9(2).
           01 LK-Stock            PIC 9(7).
           01 LK-Nombre           PIC X(30).
           01 LK-Precio-Unitario  PIC 9(5)V99.
           01 LK-Parametros.
               05 Total            PIC 9(9)v99.
               05 NombreF          PIC X(30).
               05 Tope             PIC 9(5)v99.
       PROCEDURE DIVISION USING LK-Flag,LK-Stock,LK-Nombre
       ,LK-Precio-Unitario,LK-Parametros.
       MAIN-PROCEDURE.

           PERFORM Evaluar
       EXIT PROGRAM.

           Evaluar.
               EVALUATE LK-Flag
                   WHEN 8
                       COMPUTE Total=(Total +
                       (LK-Stock * LK-Precio-Unitario))
                   WHEN 9
                       IF LK-Precio-Unitario > Tope THEN
                           MOVE LK-Precio-Unitario TO Tope
                           MOVE LK-Nombre TO NombreF
                       END-IF
                   WHEN 10
                       IF LK-Precio-Unitario < Tope THEN
                           MOVE LK-Precio-Unitario TO Tope
                           MOVE LK-Nombre   TO NombreF
                       END-IF
               END-EVALUATE
           EXIT.
