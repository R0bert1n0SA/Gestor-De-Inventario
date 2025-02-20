       IDENTIFICATION DIVISION.
       PROGRAM-ID. Carga AS "Carga".
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01  WS-Fecha                   PIC 9(8).
           01  WS-Year                    PIC 9(4)  VALUE 2000.
           01  WS-Aux                     PIC X(20).
           01  WS-NumVal                  PIC 9(10)V99.
           01  WS-Flag                    PIC 9 VALUE 1.
           01  WS-Mensaje                 PIC X(40).
       LINKAGE SECTION.
           01 LK-KeyOp  PIC 9(1).
           01 LK-Product.
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
       PROCEDURE DIVISION USING LK-KeyOp,LK-Product.
           MAIN-PROCEDURE.
               PERFORM Inicio-Busqueda
           EXIT PROGRAM.


      *>================================================================*
       *> SECCION Cargar
       *> Gestiona la carga de datos en el archivo.
      *>================================================================*
       Cargar SECTION.
           Inicio-Busqueda.
               IF LK-keyOp = 1 THEN
                   PERFORM Ingreso
                   DISPLAY "Registro guardado correctamente."
               ELSE
                   DISPLAY "Error: El producto con ID " Product-ID
                   " ya existe."
                   MOVE "no valid" TO Product-ID
               END-IF
           EXIT.

           Ingreso.
               DISPLAY "Ingrese nombre del producto: "
               ACCEPT Nombre

               MOVE "Ingrese Stock Actual: " TO WS-Mensaje
               PERFORM Validar-num
               MOVE WS-NumVal TO Stock

               MOVE "Ingrese Precio Unitario: " TO WS-Mensaje
               PERFORM Validar-num
               MOVE WS-NumVal TO Precio-Unitario


               DISPLAY "Ingrese Categoría: "
               ACCEPT Categoria
               DISPLAY "Ingrese Proveedor: "
               ACCEPT Proveedor
               ACCEPT WS-Fecha FROM DATE
               MOVE WS-Fecha(7:2) TO Dia-Registro
               MOVE WS-Fecha(5:2) TO Mes-Registro
               MOVE WS-Fecha(1:4) TO Ano-Registro
               ADD Ano-Registro TO WS-Year GIVING Ano-Registro
               MOVE 0 TO Dia-Modificacion
               MOVE 0 TO Mes-Modificacion
               MOVE 0 TO Ano-Modificacion
               DISPLAY "Ingrese Ubicación: "
               ACCEPT Ubicacion

               MOVE "Ingrese Stock Mínimo: " TO WS-Mensaje
               PERFORM Validar-num
               MOVE WS-NumVal TO Stock-Minimo

               DISPLAY "Ingrese Estado: "
               ACCEPT Estado
               DISPLAY "Ingrese Descripción: "
               ACCEPT Descripcion
               DISPLAY "Ingrese Unidad de Medida: "
               ACCEPT Unidad-Medida

               PERFORM Validar-String



           EXIT.

           Validar-num.
               PERFORM UNTIL WS-Flag = 0
                   DISPLAY WS-Mensaje
                   ACCEPT WS-Aux
                   IF FUNCTION NUMVAL(Ws-Aux) > 0 THEN
                       MOVE FUNCTION NUMVAL(Ws-Aux) TO WS-NumVal
                       INSPECT WS-NumVal REPLACING LEADING SPACES BY "0"
                       MOVE 0 TO WS-Flag
                   ELSE
                       DISPLAY "Valor no válido"

                   END-IF
               END-PERFORM
               MOVE 1 TO WS-Flag
           EXIT.

           Validar-String.
               STRING Nombre DELIMITED SIZE SPACE INTO Nombre
               STRING Categoria DELIMITED SIZE SPACE INTO Categoria
               STRING Proveedor DELIMITED SIZE SPACE INTO Proveedor
               STRING Ubicacion DELIMITED SIZE SPACE INTO Ubicacion
               STRING Estado DELIMITED SIZE SPACE INTO Estado
               STRING Descripcion DELIMITED SIZE SPACE INTO Descripcion
               STRING Unidad-Medida DELIMITED SIZE SPACE
               INTO Unidad-Medida
           EXIT.



      *>================================================================*
