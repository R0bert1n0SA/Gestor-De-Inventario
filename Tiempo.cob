       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tiempo AS "Tiempo".
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Productos ASSIGN TO 'Productos.DAT'
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
           01 WS-Ps                 PIC XX.
           01 WS-EOF-Flag           PIC X(1) VALUE 'N'.
           01 WS-Dias               PIC 9(9).
           01 WS-DiasAC             PIC 9(9).
           01 WS-DiasP              PIC 9(9).
           01 WS-RESIDUO            PIC 9(3).
           01 WS-Mod4               PIC 9(3).
           01 WS-Mod100             PIC 9(3).
           01 WS-Mod400             PIC 9(3).
           01 WS-Fecha.
              05 WS-anio            PIC 9(4).
              05 WS-mes             PIC 9(2).
              05 WS-dia             PIC 9(2).
           01 WS-Meses-Normales     PIC 9(2) OCCURS 13 TIMES.
           01 WS-Contador           PIC 9(2) VALUE 0.
           01 WS-FechaMax           PIC 9(8) VALUE 0.
           01 WS-FechaAux           PIC 9(8).
           01 WS-Producto           PIC X(30) VALUE " ".
           01 WS-FechaString        PIC X(12).
       LINKAGE SECTION.
           01 LK-Flag               PIC 9(2).
           01 LK-desact             PIC 9(3).
           01 LK-Fecha              PIC X(12).
       PROCEDURE DIVISION USING LK-Flag,LK-desact,LK-Fecha .
       MAIN-PROCEDURE.
           PERFORM Inicializar-Meses
           PERFORM Buscar
           PERFORM Reformato-Fecha
           MOVE WS-FechaString TO LK-Fecha
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





       Reformato-Fecha.
           DIVIDE WS-FechaMax BY 10000 GIVING WS-anio
           REMAINDER WS-FechaMax.
           DIVIDE WS-FechaMax BY 100 GIVING WS-mes REMAINDER WS-dia
           STRING  WS-DIA "/" WS-MES "/" WS-ANIO
               DELIMITED BY SIZE INTO WS-FechaString
           END-STRING.
           EXIT.


       Verificar-Mod.
           IF Ano-Modificacion NUMERIC AND Ano-Modificacion > 0 THEN
               MOVE 0 TO WS-Contador
           ELSE
               MOVE 1 TO WS-Contador
           END-IF
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
           MOVE 0 TO WS-Dias
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
           MOVE FUNCTION CURRENT-DATE TO WS-Fecha
           PERFORM Calculo
           MOVE WS-Dias TO WS-DiasAC

           PERFORM  Verificar-Mod
           IF WS-Contador = 0 THEN
               MOVE Ano-Modificacion TO WS-anio
               MOVE Mes-Registro TO WS-mes
               MOVE Dia-Modificacion TO WS-dia
               PERFORM Calculo
               MOVE WS-Dias TO WS-DiasP
           END-IF

           COMPUTE WS-dias =(WS-DiasAC - WS-DiasP)
           IF WS-dias > LK-desact THEN
               DISPLAY Nombre
           END-IF
           EXIT.

       Ultimo-Registro.
           PERFORM Verificar-Mod
           IF WS-Contador = 0 THEN
               COMPUTE WS-FechaAux=(Ano-Modificacion * 10000)
               +(Mes-Modificacion * 100) + Dia-Modificacion
           ELSE
               COMPUTE WS-FechaAux=(Ano-Registro * 10000)
               +(Mes-Registro * 100) + Dia-Registro
           END-IF
           IF WS-FechaAux > WS-FechaMax THEN
               MOVE WS-FechaAux TO WS-FechaMax
               MOVE Nombre TO WS-Producto
           END-IF
           EXIT.






       Buscar.
           OPEN INPUT Productos
           PERFORM UNTIL WS-EOF-Flag = 'Y'
               READ Productos INTO Product
                   AT END
                       MOVE 'Y' TO WS-EOF-Flag
                   NOT AT END
                       EVALUATE LK-Flag
                           WHEN 11
                               PERFORM Sin-Actualizacion
                           WHEN 12
                               PERFORM Ultimo-Registro
                       END-EVALUATE
               END-READ
           END-PERFORM
           CLOSE Productos
           MOVE 'N' TO WS-EOF-Flag
           EXIT.
