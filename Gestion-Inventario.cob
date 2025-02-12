       IDENTIFICATION DIVISION.
       PROGRAM-ID. Gestion-Inventario.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-opcion PIC 9(1).
           01 WS-flag PIC 9(1) VALUE 0.
           01 WS-EnterT PIC X(1).
           01 WS-Estado PIC 9(1).
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM UNTIL WS-flag = 1
               MOVE 1 TO WS-Estado
               CALL "Verificar-Archivo" USING WS-Estado
               DISPLAY "------------MENU----------------"
               DISPLAY "1. Cargar Producto"
               DISPLAY "2. Actualizar"
               DISPLAY "3. Borrar Producto"
               DISPLAY "4. Mostrar productos"
               DISPLAY "5. Sacar Estadisticas"
               DISPLAY "6. Salir"
               DISPLAY "Ingrese una Opcion: "
               ACCEPT WS-opcion
               DISPLAY X"1B" & "[2J"
               EVALUATE WS-opcion
                   WHEN 1
                       CALL "Carga"
                   WHEN 2
                       CALL "Actualizar"
                   WHEN 3
                       CALL "Eliminar"
                   WHEN 4
                       CALL "Mostrar"
                   WHEN 5
                       CALL "Estadisticas-Menu"
                       CANCEL "Estadisticas-Menu"
                   WHEN 6
                       DISPLAY "Gracias por usar la app"
                       STOP RUN
                   WHEN OTHER
                       DISPLAY "ERROR opcion no valida"
               END-EVALUATE
               PERFORM Continuar
           END-PERFORM

       STOP RUN.

       Continuar.
           IF WS-opcion = 5 THEN
               DISPLAY X"1B" & "[2J"
           ELSE
               DISPLAY "Presione Enter para continuar"
               ACCEPT WS-EnterT
               DISPLAY X"1B" & "[2J"
           END-IF
           EXIT.
