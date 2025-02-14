       IDENTIFICATION DIVISION.
       PROGRAM-ID. Mostrar AS "Mostrar".
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
           05 Product-ID           PIC X(10).
           05 P-Nombre               PIC X(30).
           05 P-Stock                PIC 9(7).
           05 P-Precio-Unitario      PIC 9(5)V99.
           05 P-Categoria            PIC X(20).
           05 P-Proveedor            PIC X(50).
           05 P-Fecha-Registro.
               10 Ano-Registro     PIC 9(4).
               10 Mes-Registro     PIC 9(2).
               10 Dia-Registro     PIC 99.
           05 P-Fecha-Modificacion.
               10 Ano-Modificacion PIC 9(4).
               10 Mes-Modificacion PIC 9(2).
               10 Dia-Modificacion PIC 99.
           05 P-Ubicacion            PIC X(50).
           05 P-Stock-Minimo         PIC 9(7).
           05 P-Estado               PIC X(10).
           05 P-Descripcion          PIC X(100).
           05 P-Unidad-Medida        PIC X(2).

       WORKING-STORAGE SECTION.
           01  WS-FileStatus        PIC XX.
           01  WS-EOF-Flag          PIC X(1) VALUE 'N'.

       PROCEDURE DIVISION.
           MAIN-PROCEDURE.
               PERFORM Iniciar
           EXIT PROGRAM.


      *================================================================*
       *> SECCION Inicio
       *> Intenta abrir el archivo de productos y maneja errores.
      *================================================================*
       Iniciar SECTION.
           Inicio.
               OPEN INPUT Productos
               IF WS-FileStatus = '00' THEN
                   PERFORM Recorrer
               ELSE
                   CALL "Errores" USING WS-FileStatus
               END-IF.
           EXIT.


           Recorrer.
               PERFORM UNTIL WS-EOF-Flag = 'Y'
                   READ Productos INTO Product
                       AT END
                           MOVE 'Y' TO WS-EOF-Flag
                       NOT AT END
                           PERFORM Imprimir
                    END-READ
               END-PERFORM
               CLOSE Productos
           EXIT.


           Imprimir.
               DISPLAY "--------------------------------"
               DISPLAY "ID: " Product-ID
               DISPLAY "Nombre: " P-Nombre
               DISPLAY "Stock Actual: " P-Stock
               DISPLAY "Precio Unitario: " P-Precio-Unitario
               DISPLAY "Categoria: " P-Categoria
               DISPLAY "Proveedor: " P-Proveedor
               DISPLAY "Fecha Registro: "
                       Dia-Registro "/" Mes-Registro
                       "/" Ano-Registro
               DISPLAY "Fecha Modificacion: "
                       Dia-Modificacion "/" Mes-Modificacion "/"
                       Ano-Modificacion
               DISPLAY "Ubicacion: " P-Ubicacion
               DISPLAY "Stock Minimo: " P-Stock-Minimo
               DISPLAY "Estado: " P-Estado
               DISPLAY "Descripcion: " P-Descripcion
               DISPLAY "Unidad de Medida: " P-Unidad-Medida
           EXIT.
      *================================================================*
