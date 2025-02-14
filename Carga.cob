       IDENTIFICATION DIVISION.
       PROGRAM-ID. Carga AS "Carga".
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
       01  WS-FileStatus       PIC XX.
       01  WS-PID                     PIC X(10).
       01  WS-Fecha                   PIC 9(8).
       01  WS-Year                    PIC 9(4)  VALUE 2000.

       PROCEDURE DIVISION.
           MAIN-PROCEDURE.
               PERFORM Inicio-Carga
           EXIT PROGRAM.


      *>================================================================*
       *> SECCION Cargar
       *> Gestiona la carga de datos en el archivo.
      *>================================================================*
       Cargar SECTION.
           Inicio-Carga.
               DISPLAY "Ingrese el ID del producto: "
               ACCEPT WS-PID
               OPEN I-O Productos
               IF WS-FileStatus = "00" THEN
                   PERFORM Inicio-Busqueda
                   CLOSE Productos
               ELSE
                   CALL "Errores" USING WS-FileStatus
               END-IF.
           EXIT.


           Inicio-Busqueda.
               MOVE WS-PID TO Product-ID
               READ Productos INTO Product KEY IS Product-ID
               INVALID KEY
                   PERFORM Ingreso
                   WRITE Product
                   DISPLAY "Registro guardado correctamente."
               NOT INVALID KEY
                   DISPLAY "Error: El producto con ID " WS-PID
                   " ya existe."
               END-READ.
           EXIT.

           Ingreso.
               DISPLAY "Ingrese nombre del producto: "
               ACCEPT Nombre
               DISPLAY "Ingrese Stock Actual: "
               ACCEPT Stock
               DISPLAY "Ingrese Precio Unitario: "
               ACCEPT Precio-Unitario
               DISPLAY "Ingrese Categoría: "
               ACCEPT Categoria
               DISPLAY "Ingrese Proveedor: "
               ACCEPT Proveedor
               ACCEPT WS-Fecha FROM DATE
               MOVE WS-Fecha(7:2) TO Dia-Registro
               MOVE WS-Fecha(5:2) TO Mes-Registro
               MOVE WS-Fecha(1:4) TO Ano-Registro
               MOVE 0 TO Dia-Modificacion
               MOVE 0 TO Mes-Modificacion
               MOVE 0 TO Ano-Modificacion
               DISPLAY "Ingrese Ubicación: "
               ACCEPT Ubicacion
               DISPLAY "Ingrese Stock Mínimo: "
               ACCEPT Stock-Minimo
               DISPLAY "Ingrese Estado: "
               ACCEPT Estado
               DISPLAY "Ingrese Descripción: "
               ACCEPT Descripcion
               DISPLAY "Ingrese Unidad de Medida: "
               ACCEPT Unidad-Medida.
           EXIT.
      *>================================================================*
