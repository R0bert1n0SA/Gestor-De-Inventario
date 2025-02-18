       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tiempo AS "Tiempo".
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-CONSTANTES.
               05 WS-AnioFACTOR        PIC 9(5) VALUE 10000.
               05 WS-MesFACTOR         PIC 9(3) VALUE 100.
           01 WS-FechaTrabajo.
              05 WS-Anio               PIC 9(4).
              05 WS-Mes                PIC 9(2).
              05 WS-Dia                PIC 9(2).
              05 WS-FechaComparacion   PIC 9(8).
           01 WS-ContadoresTiempo.
               05 WS-DiasTranscurridos PIC 9(9).
               05 WS-DiasActuales      PIC 9(9).
               05 WS-DiasPrevios       PIC 9(9).
           01 WS-Restos.
               05 WS-RESIDUO           PIC 9(3).
               05 WS-Mod4              PIC 9(3).
               05 WS-Mod100            PIC 9(3).
               05 WS-Mod400            PIC 9(3).
           01 WS-BanderaValidacion     PIC 9(2) VALUE 0.
       LINKAGE SECTION.
           01 LK-Flag                     PIC 9(2).
           01 LK-Nombre                PIC X(30).
           01 LK-Parametros.
               05 DiasDesactualizado   PIC 9(3).
               05 R-Fecha.
                   10 RF-Anio          PIC 9(4).
                   10 RF-Mes           PIC 9(2).
                   10 RF-Dia           PIC 9(2).
               05 M-Fecha.
                   10 MF-Anio          PIC 9(4).
                   10 MF-Mes           PIC 9(2).
                   10 MF-Dia           PIC 9(2).
               05 DiasPorMes           PIC 9(2) OCCURS 13 TIMES.
               05 FechaMasReciente     PIC 9(8) VALUE 0.
               05 FechaString          PIC X(12).

       PROCEDURE DIVISION USING LK-Flag,LK-Nombre,LK-Parametros.
       MAIN-PROCEDURE.
           PERFORM Procesar
       EXIT PROGRAM.

      *================================================================*
       *> SECCION Procesar
      *================================================================*
       S-Procesar SECTION.
           Procesar.
               PERFORM Procesar-Registros
               PERFORM Reformatear-Fecha
           EXIT.


       *> Procesamiento de registros
           Procesar-Registros.
               EVALUATE LK-Flag
                   WHEN 11
                       PERFORM Analizar-Desactualizacion
                   WHEN 12
                       PERFORM Analizar-Ultimo-Registro
               END-EVALUATE
           EXIT.
      *================================================================*


      *================================================================*
       *> SECCION Desactualizado
      *================================================================*
       *> Análisis de desactualización
       Desactualizado SECTION.
           Analizar-Desactualizacion.
               PERFORM Calcular-Dias-Actuales
               PERFORM Calcular-Dias-Previos
               PERFORM Evaluar-Desactualizacion
           EXIT.
          *>------------------------------------------------------------
           *> SUBSECCION de calculos
          *>------------------------------------------------------------
           Calcular SECTION.

               Verificar-Bisiesto.
                   DIVIDE WS-Anio BY 4 GIVING WS-RESIDUO
                   REMAINDER WS-Mod4.
                   DIVIDE WS-Anio BY 100 GIVING WS-RESIDUO
                   REMAINDER WS-Mod100.
                   DIVIDE WS-Anio BY 400 GIVING WS-RESIDUO
                   REMAINDER WS-Mod400.
                   IF (WS-Mod4 = 0 AND WS-Mod100 > 0) OR WS-Mod400 = 0
                   THEN
                       MOVE 0 TO WS-RESIDUO
                   ELSE
                       MOVE 1 TO WS-RESIDUO
                   END-IF
               EXIT.


               Sumar-Dias-Del-Meses.
                   PERFORM VARYING WS-BanderaValidacion FROM 1 BY 1
                   UNTIL WS-BanderaValidacion > WS-mes
                       IF WS-BanderaValidacion = 2 AND WS-RESIDUO = 0
                       THEN
                           COMPUTE WS-DiasTranscurridos=
                           (WS-DiasTranscurridos + DiasPorMes(13))
                       ELSE
                           COMPUTE WS-DiasTranscurridos=
                           (WS-DiasTranscurridos + DiasPorMes
                           (WS-BanderaValidacion))
                       END-IF
                   END-PERFORM
               EXIT.


               Agregar-Dias-Anio.
                   IF WS-RESIDUO = 0 THEN
                       COMPUTE WS-DiasTranscurridos=WS-DiasTranscurridos
                       + (WS-anio * 366) + WS-dia
                   ELSE
                       COMPUTE WS-DiasTranscurridos=WS-DiasTranscurridos
                       + (WS-anio * 365) + WS-dia
                   END-IF
               EXIT.


               Calculo.
                   MOVE 0 TO WS-DiasTranscurridos
                   IF WS-Anio NUMERIC AND WS-Anio > 0 THEN
                       PERFORM Verificar-Bisiesto
                       PERFORM Sumar-Dias-Del-Meses
                       PERFORM Agregar-Dias-Anio
                   END-IF
               EXIT.


               Calcular-Dias-Actuales.
                   MOVE FUNCTION CURRENT-DATE TO WS-FechaTrabajo
                   PERFORM Calculo
                   MOVE WS-DiasTranscurridos TO WS-DiasActuales
               EXIT.


               Calcular-Dias-Previos.
                   IF MF-Anio NOT NUMERIC OR
                   MF-Anio = 0 THEN
                       MOVE 0 TO WS-DiasPrevios
                   ELSE
                       MOVE MF-Anio    TO WS-Anio
                       MOVE MF-Mes     TO WS-Mes
                       MOVE MF-Dia     TO WS-Dia
                       PERFORM Calculo
                       MOVE WS-DiasTranscurridos TO WS-DiasPrevios
                   END-IF
               EXIT.
          *>-----------------------------------------------------------

           Evaluar-Desactualizacion.
               COMPUTE WS-DiasTranscurridos =(WS-DiasActuales
               - WS-DiasPrevios)
               IF WS-DiasTranscurridos > DiasDesactualizado THEN
                   DISPLAY LK-Nombre
               END-IF
           EXIT.
      *================================================================*


      *================================================================*
       *>SECCION  Ultimo-Registro
      *================================================================*
       Ultimo-Registro SECTION.
       *> Análisis de último registro
           Analizar-Ultimo-Registro.
               IF MF-Anio NUMERIC AND MF-Anio > 0 THEN
                   PERFORM Calcular-Fecha-Modificacion
               ELSE
                   PERFORM Calcular-Fecha-Registro
               END-IF
               IF WS-FechaComparacion > FechaMasReciente THEN
                   MOVE WS-FechaComparacion TO FechaMasReciente
               END-IF
           EXIT.

           Calcular-Fecha-Modificacion.
               COMPUTE WS-FechaComparacion =
                      (MF-Anio * WS-AnioFACTOR) +
                      (MF-Mes * WS-MesFACTOR) +
                      MF-Dia
           EXIT.


           Calcular-Fecha-Registro.
               COMPUTE WS-FechaComparacion =
                      (RF-Anio * WS-AnioFACTOR) +
                      (RF-Mes * WS-MesFACTOR) +
                       RF-Dia
           EXIT.
      *================================================================*


      *================================================================*
      *>SECCION Reformato
      *================================================================*
       Reformato SECTION.
       *> Reformateo de fecha para salida
           Reformatear-Fecha.
               DIVIDE FechaMasReciente BY 10000 GIVING WS-Anio
               REMAINDER FechaMasReciente.
               DIVIDE FechaMasReciente BY 100 GIVING WS-Mes
               REMAINDER WS-Dia
               STRING  WS-Dia "/" WS-Mes "/" WS-Anio
                   DELIMITED BY SIZE INTO FechaString
               END-STRING.
           EXIT.
      *================================================================*
