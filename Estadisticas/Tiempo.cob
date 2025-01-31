       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tiempo AS "Tiempo".
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Productos
           ASSIGN TO
           'F:\Proyectos\Cobol\Gestion de Inventarios\bin\Productos.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS Product-ID
               FILE STATUS IS WS-Ps.
       DATA DIVISION.
       FILE SECTION.
       FD  Productos.
       01  Product.
           05 Product-ID           PIC X(10).
           05 Nombre               PIC X(30).
           05 Stock                PIC 9(7).
           05 Precio-Unitario      PIC 9(5)V99.
           05 Categoria            PIC X(20).
           05 Proveedor            PIC X(50).
           05 Fecha-Registro.
               10 Ano-Registro     PIC 9(4).
               10 Mes-Registro     PIC 9(2).
               10 Dia-Registro     PIC 99.
           05 Fecha-Modificacion.
               10 Ano-Modificacion PIC 9(4).
               10 Mes-Modificacion PIC 9(2).
               10 Dia-Modificacion PIC 99.
           05 Ubicacion            PIC X(50).
           05 Stock-Minimo         PIC 9(7).
           05 Estado               PIC X(10).
           05 Descripcion          PIC X(100).
           05 Unidad-Medida        PIC X(2).

       WORKING-STORAGE SECTION.
           01 WS-Ps       PIC XX.
           01 WS-EOF-Flag PIC X(1) VALUE 'N'.
           01 WS-Dias     PIC 9(9).
           01 WS-DiasAC   PIC 9(9).
           01 WS-DiasP    PIC 9(9).
           01 WS-RESIDUO  PIC 9(3).
           01 WS-Mod4     PIC 9(3).
           01 WS-Mod100   PIC 9(3).
           01 WS-Mod400   PIC 9(3).
           01 WS-Fecha.
              05 WS-anio  PIC 9(4).
              05 WS-mes   PIC 9(2).
              05 WS-dia   PIC 9(2).
           01 WS-Tabla-Meses.
               05 WS-Meses-Normales     PIC 9(2) OCCURS 13 TIMES.
           01 WS-Contador PIC 9(2) VALUE 0.
       LINKAGE SECTION.
           01 LK-Flag     PIC 9(2).
           01 LK-desact   PIC 9(3).
       PROCEDURE DIVISION USING LK-Flag.
       MAIN-PROCEDURE.
           PERFORM Inicializar-Meses
           PERFORM Sin-Actualizacion
       EXIT PROGRAM.

       Inicializar-Meses.
           MOVE 31 TO  WS-Meses-Normales(1)
                       WS-Meses-Normales(3)
                       WS-Meses-Normales(5)
                       WS-Meses-Normales(7)
                       WS-Meses-Normales(8)
                       WS-Meses-Normales(10)
                       WS-Meses-Normales(12)
           MOVE 30 TO  WS-Meses-Normales(4)
                       WS-Meses-Normales(6)
                       WS-Meses-Normales(9)
                       WS-Meses-Normales(11)
           MOVE 28 TO  WS-Meses-Normales(2)
           MOVE 29 TO  WS-Meses-Normales(13)
           EXIT.





       Meses.
           PERFORM VARYING WS-Contador FROM 1 BY 1
           UNTIL WS-Contador > WS-mes
               IF WS-Contador = 2 AND WS-RESIDUO = 0 THEN
                   COMPUTE WS-Dias=(WS-Dias +
                   WS-Meses-Normales(13))
               ELSE
                   COMPUTE WS-Dias=(WS-Dias +
                   WS-Meses-Normales(WS-Contador))
               END-IF
           END-PERFORM
           EXIT.

       Anio-Bisiesto.
           DIVIDE WS-anio BY 4   GIVING WS-RESIDUO REMAINDER WS-Mod4.
           DIVIDE WS-anio BY 100 GIVING WS-RESIDUO REMAINDER WS-Mod100.
           DIVIDE WS-anio BY 400 GIVING WS-RESIDUO REMAINDER WS-Mod400.
           IF (WS-Mod4 = 0 AND WS-Mod100 > 0) OR WS-Mod400 = 0 THEN
               MOVE 0 TO WS-RESIDUO
           ELSE
               MOVE 1 TO WS-RESIDUO
           END-IF
           EXIT.


       Calculo.
           IF WS-anio > 0 THEN
               PERFORM Anio-Bisiesto
               IF WS-RESIDUO = 0 THEN
                   COMPUTE WS-Dias=WS-Dias + (WS-anio * 366) + WS-dia
               ELSE
                   COMPUTE WS-Dias=WS-Dias + (WS-anio * 365) + WS-dia
               END-IF
           END-IF
           EXIT.


       Sin-Actualizacion.
           OPEN INPUT Productos
           PERFORM UNTIL WS-EOF-Flag = 'Y'
               READ Productos INTO Product
                   AT END
                       MOVE 'Y' TO WS-EOF-Flag
                   NOT AT END
                       EVALUATE LK-Flag
                           WHEN 11
                               MOVE FUNCTION CURRENT-DATE TO WS-Fecha
                               PERFORM Calculo
                               MOVE WS-Dias TO WS-DiasAC
                               MOVE 0 TO WS-Dias

                               MOVE Ano-Modificacion TO WS-anio
                               MOVE Mes-Registro TO WS-mes
                               MOVE Dia-Modificacion TO WS-dia
                               PERFORM Calculo
                               MOVE WS-Dias TO WS-DiasP
                               MOVE 0 TO WS-Dias

                               COMPUTE WS-dias =(WS-DiasAC - WS-DiasP)
                               IF WS-dias > LK-desact THEN
                                   DISPLAY Nombre
                               END-IF
                           WHEN 12
                               IF
                       END-EVALUATE
               END-READ
           END-PERFORM
           CLOSE Productos
           EXIT.
