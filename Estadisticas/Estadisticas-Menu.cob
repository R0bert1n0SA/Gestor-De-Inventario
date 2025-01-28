       IDENTIFICATION DIVISION.
       PROGRAM-ID. Estadisticas-Menu AS "Estadisticas".
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 flag PIC 9(1).
           01 opcion PIC 9(2).
           01 Total PIC 9(9).
           01 tecla PIC X(1).
           01 Nombre PIC X(30).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL flag = 1
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
                           "añadido/modificado"
               DISPLAY "0. Menu principal"
               DISPLAY "Ingrese una Opcion: "
               ACCEPT opcion
               DISPLAY X"1B" & "[2J"
               EVALUATE opcion
                   WHEN 1
                       CALL "General" USING opcion,Total
                       DISPLAY "Productos Registrados: "Total
                       DISPLAY "Presione Enter para continuar"
                       ACCEPT tecla
                   WHEN 2
                       CALL "General" USING opcion,Total
                       DISPLAY "Stock total General: "Total
                       DISPLAY "Presione Enter para continuar"
                       ACCEPT tecla
                   WHEN 3
                       CALL "Productos" USING opcion,Nombre
                       DISPLAY "Stock total General: " Nombre
                       DISPLAY "Presione Enter para continuar"
                       ACCEPT tecla
                   WHEN 4
                       CALL "Productos" USING opcion,Nombre
                       DISPLAY "Stock total General: " Nombre
                       DISPLAY "Presione Enter para continuar"
                       ACCEPT tecla
                   WHEN 5
                       CALL "Productos" USING opcion,"y"
                       DISPLAY "Presione Enter para continuar"
                       ACCEPT tecla
                   WHEN 6
                       CALL "Categoria" USING opcion
                       DISPLAY "Presione Enter para continuar"
                       ACCEPT tecla
                   WHEN 7
                       CALL "Categoria" USING opcion
                       DISPLAY "Presione Enter para continuar"
                       ACCEPT tecla
                   WHEN 0
                       Move 1 to flag
                   WHEN OTHER
                       DISPLAY "ERROR opcion no valida"
                       DISPLAY "Presione Enter para continuar"
                       ACCEPT tecla
               END-EVALUATE
           END-PERFORM
       EXIT PROGRAM.
