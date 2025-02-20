       IDENTIFICATION DIVISION.
       PROGRAM-ID. Eliminar AS "Eliminar".
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
           01 LK-KeyOp                PIC 9(1).
           01 LK-Product-ID           PIC X(10).

       PROCEDURE DIVISION USING LK-KeyOp,LK-Product-ID.
           MAIN-PROCEDURE.
               PERFORM Eliminar
           EXIT PROGRAM.


       Eliminar.
           IF LK-keyOp = 2 THEN
               DISPLAY "Producto eliminado exitosamente."
           ELSE
               DISPLAY "Error: El producto con ID " LK-Product-ID
               " No Existe."
               MOVE "no valid" TO LK-Product-ID
           END-IF
       EXIT.
