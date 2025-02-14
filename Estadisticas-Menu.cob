       IDENTIFICATION DIVISION.
       PROGRAM-ID. Estadisticas-Menu AS "Estadisticas-Menu".
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-flag   PIC 9(1) VALUE 0.
           01 WS-opcion PIC S9(2).
           01 WS-Input  PIC X(3).
           01 WS-Total  PIC 9(9).
           01 WS-Costo  PIC 9(9)v99.
           01 WS-Top    PIC 9(5)v99.
           01 WS-tecla  PIC X(1).
           01 WS-Nombre PIC X(30).
           01 WS-dias   PIC 9(3).
           01 WS-Temp   PIC S9(2).
           01 WS-Fecha  PIC X(12) .

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL WS-flag = 1
               DISPLAY "------------Estadisticas----------------"
               DISPLAY "1.Total de productos registrados"
               DISPLAY "2.Stock total General"
               DISPLAY "3.Producto con mayor stock"
               DISPLAY "4.Producto con menor stock"
               DISPLAY "5.Productos bajo stock minimo"
               DISPLAY "6.Cantidad de productos por categoria "
               DISPLAY "7.Categoria con mas productos registrados"
               DISPLAY "8.Valor total del inventario "
               DISPLAY "9.Producto mas caro"
               DISPLAY "10.Producto mas barato"
               DISPLAY "11.Productos sin actualizacion reciente"
               DISPLAY "12.Fecha del ultimo registro"
                           " aniadido/modificado"
               DISPLAY "0. Menu principal"
               DISPLAY "Ingrese una Opcion: "
               ACCEPT WS-Input
               PERFORM Verificar
               DISPLAY X"1B" & "[2J"
               EVALUATE WS-opcion
                   WHEN 1
                       CALL "General" USING WS-opcion,WS-Total
                       DISPLAY "Productos Registrados: "WS-Total
                   WHEN 2
                       CALL "General" USING WS-opcion,WS-Total
                       DISPLAY "Stock total General: "WS-Total
                   WHEN 3
                       CALL "Productos" USING WS-opcion,WS-Nombre
                       DISPLAY "Producto con mas stock: " WS-Nombre
                   WHEN 4
                       CALL "Productos" USING WS-opcion,WS-Nombre
                       DISPLAY "Producto con menor Stock: " WS-Nombre
                   WHEN 5
                       CALL "Productos" USING WS-opcion,WS-Nombre
                       DISPLAY "---------Productos con Bajo Stock------"
                       "------"
                   WHEN 6
                       CALL "Categoria" USING WS-opcion
                   WHEN 7
                       CALL "Categoria" USING WS-opcion
                   WHEN 8
                       MOVE " " TO WS-Nombre
                       MOVE 0 TO WS-Top
                       CALL "Finanzas" USING WS-opcion,WS-Nombre
                       ,WS-Costo,WS-Top
                       DISPLAY "Costo Total de Inventario: "WS-Costo
                   WHEN 9
                       MOVE 0 TO WS-Top
                       MOVE 0 TO WS-Costo
                       CALL "Finanzas" USING WS-opcion,WS-Nombre
                       ,WS-Costo,WS-Top
                       DISPLAY "EL Producto Mas Caro es: " WS-Nombre
                      " Precio: "WS-Top
                   WHEN 10
                       MOVE 99999 TO WS-Top
                       MOVE 0 TO WS-Costo
                       CALL "Finanzas" USING WS-opcion,WS-Nombre
                       ,WS-Costo,WS-Top
                       DISPLAY "EL Producto Mas Barato es: "WS-Nombre
                       " Precio: "WS-Top
                   WHEN 11
                       MOVE WS-opcion TO WS-Temp
                       PERFORM Opcion_11
                   WHEN 12
                       CALL "Tiempo" USING WS-opcion,WS-dias,WS-Fecha
                       DISPLAY "Fecha del ultimo registro: "WS-Fecha
                   WHEN 0
                       DISPLAY X"1B" & "[2J"
                       MOVE 1 TO WS-flag
                   WHEN OTHER
                       DISPLAY "ERROR caracter no valido"
               END-EVALUATE
                   PERFORM Continuar
               DISPLAY X"1B" & "[2J"
           END-PERFORM
       EXIT PROGRAM.



       Verificar.
           IF FUNCTION TEST-NUMVAL(WS-Input) = 0 THEN
               MOVE FUNCTION NUMVAL(WS-Input) TO WS-opcion
           ELSE
               MOVE -99 TO WS-opcion
           END-IF
           EXIT.

       Opcion_11.
           PERFORM UNTIL WS-flag = 1
               DISPLAY "Cuantos dias se considera no"
               "actualizado recientemente [1 a 30]: "
               ACCEPT WS-Input
               PERFORM Verificar
               MOVE WS-opcion TO WS-dias
               IF WS-dias >= 1 and WS-dias <= 31 THEN
                   MOVE WS-Temp TO WS-opcion
                   CALL "Tiempo" USING WS-opcion,WS-dias,WS-Fecha
                   MOVE 1 TO WS-flag
               ELSE
                   DISPLAY "Valor no valido"
               END-IF
           END-PERFORM
           MOVE 0 TO WS-flag
           EXIT.

       Continuar.
           IF WS-opcion = 0 THEN
               GOBACK
           ELSE
               DISPLAY "Presione Enter para continuar"
               ACCEPT WS-tecla
           END-IF
           EXIT.
