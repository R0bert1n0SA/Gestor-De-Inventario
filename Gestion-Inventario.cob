       IDENTIFICATION DIVISION.
       PROGRAM-ID. Gestion-Inventario.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Productos ASSIGN TO 'Productos.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS Product-ID
               FILE STATUS IS File-Status.
       DATA DIVISION.
       FILE SECTION.
       FD  Productos.
       01  Product.
           05 Product-ID             PIC X(10).
           05 P-Nombre               PIC X(30).
           05 P-Stock                PIC 9(7).
           05 P-Precio-Unitario      PIC 9(5)V99.
           05 P-Categoria            PIC X(20).
           05 P-Proveedor            PIC X(50).
           05 P-Fecha-Registro.
               10 Ano-Registro       PIC 9(4).
               10 Mes-Registro       PIC 9(2).
               10 Dia-Registro       PIC 99.
           05 P-Fecha-Modificacion.
               10 Ano-Modificacion   PIC 9(4).
               10 Mes-Modificacion   PIC 9(2).
               10 Dia-Modificacion   PIC 99.
           05 P-Ubicacion            PIC X(50).
           05 P-Stock-Minimo         PIC 9(7).
           05 P-Estado               PIC X(10).
           05 P-Descripcion          PIC X(100).
           05 P-Unidad-Medida        PIC X(2).

       WORKING-STORAGE SECTION.
           01 WS-Control.
               05 File-Status        PIC XX.
               05 R-EOF              PIC X(1).
               05 R-Estados          PIC X(1).
               05 R-KeyOpcion        PIC 9(1).
           01 WS-opcion              PIC 9(1).
           01 WS-flag                PIC 9(1) VALUE 0.
           01 WS-EnterT              PIC X(1).
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM Menu-gestor
       STOP RUN.

           Verificar-Producto.
               OPEN I-O Productos
               IF File-Status = '00' THEN
                   PERFORM Inicializar
                   PERFORM Recorrido-Generico
                   CLOSE Productos
               ELSE
                   CALL "Errores" USING File-Status
               END-IF
           EXIT.


           Inicializar.
               MOVE 'N' TO R-EOF
           EXIT.




      *>================================================================*
       *>  Seccion Menu
       *>  Funciones del menu: Bucle , Muestra y Evaluacion de opciones
      *>================================================================*
       Menu SECTION.
           Menu-gestor.
               PERFORM UNTIL WS-flag = 1
                   CALL "Verificar-Archivo"
                   PERFORM Muestra-Opciones
                   PERFORM Evaluar
                   PERFORM Continuar
               END-PERFORM
           EXIT.


           Muestra-Opciones.
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
           EXIT.


           Evaluar.
               EVALUATE WS-opcion
                   WHEN 1 THRU 4
                       PERFORM Verificar-Producto
                   WHEN 5
                       CALL  "Estadisticas-Menu"
                       CANCEL"Estadisticas-Menu"
                   WHEN 6
                       DISPLAY "Gracias por usar la app"
                       STOP RUN
                   WHEN OTHER
                       DISPLAY "ERROR opcion no valida"
               END-EVALUATE
           EXIT.


           Continuar.
               IF WS-opcion = 5 THEN
                   DISPLAY X"1B" & "[2J"
               ELSE
                   DISPLAY "Presione Enter para continuar"
                   ACCEPT WS-EnterT
                   DISPLAY X"1B" & "[2J"
               END-IF
           EXIT.
      *>================================================================*



      *>================================================================*
       *>  Seccion Menu
       *>  Funciones del menu: Bucle , Muestra y Evaluacion de opciones
      *>================================================================*
       Recorrer SECTION.
           Recorrido-Generico.
               IF WS-opcion = 4 THEN
                   PERFORM Recorrido-Lectura
               ELSE
                   PERFORM Recorrido-Escritura
               END-IF
           EXIT.


           Recorrido-Lectura.
               PERFORM UNTIL R-EOF = 'Y'
                   READ Productos INTO Product
                       AT END
                           MOVE 'Y' TO R-EOF
                       NOT AT END
                           CALL  "Mostrar" USING Product
                   END-READ
               END-PERFORM
               MOVE 'N' TO R-EOF
           EXIT.


           Recorrido-Escritura.
               DISPLAY "Ingrese el ID del producto: "
               ACCEPT Product-ID
               READ Productos INTO Product KEY IS Product-ID
                   INVALID KEY
                       MOVE 1 TO R-KeyOpcion
                       PERFORM Accion
                   NOT INVALID KEY
                       MOVE 2 TO R-KeyOpcion
                       PERFORM Accion
               END-READ
           EXIT.


           Accion.
               EVALUATE WS-opcion
                   WHEN 1
                       CALL "Carga"    USING R-KeyOpcion,Product
                       IF Product-ID NOT = "no valid" THEN
                           WRITE Product
                       END-IF
                   WHEN 2
                       CALL "Actualizacion"   USING R-KeyOpcion,Product
                       IF Product-ID NOT = "no valid" THEN
                           REWRITE Product
                       END-IF
                   WHEN 3
                       CALL "Eliminar"   USING R-KeyOpcion,Product-ID
                       IF Product-ID NOT = "no valid" THEN
                           DELETE Productos
                       END-IF
               END-EVALUATE
           EXIT.
