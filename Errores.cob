       IDENTIFICATION DIVISION.
       PROGRAM-ID. Errores As "Errores".
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-NotFound     PIC XX.
           01 WS-Incompatiple PIC XX.
       LINKAGE SECTION.
           01 LK-Flag    PIC XX.
       PROCEDURE DIVISION USING LK-Flag .
       MAIN-PROCEDURE.
           PERFORM Detect-Error
       EXIT PROGRAM.

      *>---------------------------------------------------------------
       *> M�dulo: Manejo-Errores
       *> Descripci�n: Muestra errores espec�ficos seg�n FileStatus.
       *>---------------------------------------------------------------
       Detect-Error.
           EVALUATE LK-Flag
               WHEN WS-NotFound
                   DISPLAY "ERROR: Archivo no encontrado."
               WHEN WS-Incompatiple
                   DISPLAY "ERROR: Incompatibilidad en la estructura "
                   "del archivo."
               WHEN OTHER
                   DISPLAY "ERROR DESCONOCIDO: " LK-Flag
           END-EVALUATE.
       EXIT.
