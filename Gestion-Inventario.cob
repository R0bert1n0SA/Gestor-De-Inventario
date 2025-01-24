       IDENTIFICATION DIVISION.
           PROGRAM-ID. Gestion-Inventario.
           DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 opcion PIC 9(1).
           01 flag PIC 9(1) VALUE 0.
           01 EnterT PIC X(1).
           PROCEDURE DIVISION.
           MAIN-PROGRAM.
               PERFORM UNTIL flag = 1
                   CALL "Verificar-Archivo"
                   DISPLAY "------------MENU----------------"
                   DISPLAY "1. Cargar Producto"
                   DISPLAY "2. Actualizar"
                   DISPLAY "3. Borrar Producto"
                   DISPLAY "4. Mostrar productos"
                   DISPLAY "5. Sacar Estadisticas"
                   DISPLAY "6. Salir"
                   DISPLAY "Ingrese una Opcion: "
                   ACCEPT opcion
                   DISPLAY X"1B" & "[2J"
                   EVALUATE opcion
                       WHEN 1
                           CALL "Carga"
                       WHEN 2
                           CALL "Actualizar"
                       WHEN 3
                           CALL "Eliminar"
                       WHEN 4
                           CALL "Mostrar"
                       WHEN 5
                           CALL "Estadisticas"
                       WHEN 6
                           DISPLAY "Gracias por usar la app"
                           MOVE 1 TO flag
                       WHEN OTHER
                           DISPLAY "ERROR opcion no valida"
                           DISPLAY "Presione Enter para continuar"
                           ACCEPT EnterT
                           DISPLAY X"1B" & "[2J"
                   END-EVALUATE
               END-PERFORM

           STOP RUN.
