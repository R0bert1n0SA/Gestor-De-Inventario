       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tiempo AS "Tiempo".
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Productos ASSIGN TO 'Productos.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS Product-ID
               FILE STATUS IS WS-FileStatus.
       DATA DIVISION.
       FILE SECTION.
       FD  Productos.
       01  Product.
           05 Product-ID               PIC X(10).
           05 P-Nombre                 PIC X(30).
           05 P-Fecha-Registro.
               10 Registro-Ano         PIC 9(4).
               10 Registro-Mes         PIC 9(2).
               10 Registro-Dia         PIC 9(2).
           05 P-Fecha-Modificacion.
               10 Modificacion-Ano     PIC 9(4).
               10 Modificacion-Mes     PIC 9(2).
               10 Modificacion-Dia     PIC 9(2).


       WORKING-STORAGE SECTION.
           01 WS-Control.
               05 WS-FileStatus        PIC XX.
               05 WS-EOF-Flag          PIC X(1) VALUE 'N'.
           01 WS-CONSTANTES.
               05 WS-Anio-FACTOR       PIC 9(5) VALUE 10000.
               05 WS-Mes-FACTOR        PIC 9(3) VALUE 100.
           01 WS-ContadoresTiempo.
               05 WS-DiasTranscurridos PIC 9(9).
               05 WS-DiasActuales      PIC 9(9).
               05 WS-DiasPrevios       PIC 9(9).
           01 WS-Restos.
               05 WS-RESIDUO           PIC 9(3).
               05 WS-Mod4              PIC 9(3).
               05 WS-Mod100            PIC 9(3).
               05 WS-Mod400            PIC 9(3).
           01 WS-FechaTrabajo.
              05 WS-Anio               PIC 9(4).
              05 WS-Mes                PIC 9(2).
              05 WS-Dia                PIC 9(2).
           01 DatosAnalisis.
               05 WS-FechaMasReciente  PIC 9(8) VALUE 0.
               05 WS-FechaComparacion  PIC 9(8).
               05 WS-ProductoReciente  PIC X(30) VALUE SPACES.
               05 WS-FechaString       PIC X(12).
           01 WS-Flag                  PIC 9(1).
           01 WS-BanderaValidacion     PIC 9(2) VALUE 0.
           01 WS-DiasPorMes            PIC 9(2) OCCURS 13 TIMES.
       LINKAGE SECTION.
           01 LK-Flag                  PIC 9(2).
           01 LK-DiasDesactualizado    PIC 9(3).
           01 LK-Fecha                 PIC X(12).
       PROCEDURE DIVISION USING LK-Flag,LK-DiasDesactualizado,LK-Fecha.
       MAIN-PROCEDURE.
           PERFORM Inicio
       EXIT PROGRAM.


      *================================================================*
       *> SECCION INICIO
      *================================================================*
       Iniciar SECTION.
       *> Inicialización de la tabla de días por mes
           InicializarDiasPorMes.
               MOVE 31 TO  WS-DiasPorMes(1),
                           WS-DiasPorMes(3),
                           WS-DiasPorMes(5),
                           WS-DiasPorMes(7),
                           WS-DiasPorMes(8),
                           WS-DiasPorMes(10),
                           WS-DiasPorMes(12)

               MOVE 30 TO  WS-DiasPorMes(4),
                           WS-DiasPorMes(6),
                           WS-DiasPorMes(9),
                           WS-DiasPorMes(11)

               MOVE 28 TO  WS-DiasPorMes(2)
               MOVE 29 TO  WS-DiasPorMes(13)
           EXIT.


           Inicio.
               PERFORM InicializarDiasPorMes
               PERFORM Procesar
           EXIT.
      *================================================================*


      *================================================================*
       *> SECCION Procesar
      *================================================================*
       S-Procesar SECTION.
           Procesar.
               PERFORM Procesar-Registros
               PERFORM Reformatear-Fecha
               MOVE WS-FechaString TO LK-Fecha
           EXIT.


       *> Procesamiento de registros
           Procesar-Registros.
               OPEN INPUT Productos
               PERFORM UNTIL WS-EOF-Flag = 'Y'
                   READ Productos INTO Product
                       AT END
                           MOVE 'Y' TO WS-EOF-Flag
                           DISPLAY "fin archivo"
                       NOT AT END
                           EVALUATE LK-Flag
                               WHEN 11
                                   PERFORM Analizar-Desactualizacion
                               WHEN 12
                                   PERFORM Analizar-Ultimo-Registro
                           END-EVALUATE
                   END-READ
               END-PERFORM
               CLOSE Productos
               MOVE 'N' TO WS-EOF-Flag
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
                           (WS-DiasTranscurridos +WS-DiasPorMes(13))
                       ELSE
                           COMPUTE WS-DiasTranscurridos=
                           (WS-DiasTranscurridos + WS-DiasPorMes
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
                   IF Modificacion-Ano NOT NUMERIC OR
                   Modificacion-Ano = 0 THEN
                       MOVE 0 TO WS-DiasPrevios
                   ELSE
                       MOVE Modificacion-Ano TO WS-Anio
                       MOVE Modificacion-Mes TO WS-Mes
                       MOVE Modificacion-Dia TO WS-Dia
                       PERFORM Calculo
                       MOVE WS-DiasTranscurridos TO WS-DiasPrevios
                   END-IF
               EXIT.
          *>-----------------------------------------------------------

           Evaluar-Desactualizacion.
               COMPUTE WS-DiasTranscurridos =(WS-DiasActuales
               - WS-DiasPrevios)
               IF WS-DiasTranscurridos > LK-DiasDesactualizado THEN
               DISPLAY P-Nombre
               END-IF
           EXIT.
      *================================================================*


      *================================================================*
       *>SECCION  Ultimo-Registro
      *================================================================*
       Ultimo-Registro SECTION.
       *> Análisis de último registro
           Analizar-Ultimo-Registro.
               IF Modificacion-Ano NUMERIC AND Modificacion-Ano > 0 THEN
                   PERFORM Calcular-Fecha-Modificacion
               ELSE
                   PERFORM Calcular-Fecha-Registro
               END-IF
               IF WS-FechaComparacion > WS-FechaMasReciente THEN
                   MOVE WS-FechaComparacion TO WS-FechaMasReciente
                   MOVE P-Nombre TO WS-ProductoReciente
               END-IF
           EXIT.

           Calcular-Fecha-Modificacion.
               COMPUTE WS-FechaComparacion =
                      (Modificacion-Ano * WS-Anio-FACTOR) +
                      (Modificacion-Mes * WS-Mes-FACTOR) +
                      Modificacion-Dia
           EXIT.


           Calcular-Fecha-Registro.
               COMPUTE WS-FechaComparacion =
                      (Registro-Ano * WS-Anio-FACTOR) +
                      (Registro-Mes * WS-Mes-FACTOR) +
                       Registro-Dia
           EXIT.
      *================================================================*


      *================================================================*
      *>SECCION Reformato
      *================================================================*
       Reformato SECTION.
       *> Reformateo de fecha para salida
           Reformatear-Fecha.
               DIVIDE WS-FechaMasReciente BY 10000 GIVING WS-Anio
               REMAINDER WS-FechaMasReciente.
               DIVIDE WS-FechaMasReciente BY 100 GIVING WS-Mes
               REMAINDER WS-Dia
               STRING  WS-Dia "/" WS-Mes "/" WS-Anio
                   DELIMITED BY SIZE INTO WS-FechaString
               END-STRING.
           EXIT.
      *================================================================*
