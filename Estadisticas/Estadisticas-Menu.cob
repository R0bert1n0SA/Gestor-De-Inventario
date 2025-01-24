       IDENTIFICATION DIVISION.
       PROGRAM-ID. Estadisticas-Menu AS "Estadisticas".
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 flag PIC 9(1).
           01 opcion PIC 9(1).
           01 Total PIC 9(9).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM UNTIL flag = 1
                   DISPLAY "------------Estadisticas----------------"
                   DISPLAY "1.Total de productos registrados"
                   DISPLAY "2.Cantidad total de unidades"
                   DISPLAY "3.Producto con mayor stock"
                   DISPLAY "4.Producto con menor stock"
                   DISPLAY "5.Productos bajo stock mínimo"
                   DISPLAY "6.Cantidad de productos por categoría "
                   DISPLAY "7.Categoría con más productos registrados"
                   DISPLAY "8.Valor total del inventario "
                   DISPLAY "9.Producto más caro"
                   DISPLAY "10.Producto más barato"
                   DISPLAY "11.Productos sin actualización reciente"
                   DISPLAY "12.Fecha del último registro"
                           "añadido/modificado"
                   DISPLAY "0. Menu principal"
                   DISPLAY "Ingrese una Opcion: "
                   ACCEPT opcion
                   DISPLAY X"1B" & "[2J"
                   EVALUATE opcion
                       WHEN 1
                           CALL "General" USING opcion,Total
                       WHEN 2
                           CALL "Actualizar"
                       WHEN 3
                           CALL "Eliminar"
                       WHEN 4
                           CALL "Mostrar"
                       WHEN 5
                           DISPLAY "Estadisticas"
                       WHEN OTHER
                           DISPLAY "ERROR opcion no valida"
                           DISPLAY "Presione Enter para continuar"
                   END-EVALUATE
               END-PERFORM
       EXIT PROGRAM.
