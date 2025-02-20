       IDENTIFICATION DIVISION.
       PROGRAM-ID. Actualizacion AS "Actualizar".
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-Fecha            PIC 9(8).
           01 WS-Year             PIC 9(4)  VALUE 2000.
       LINKAGE SECTION.
           01 LK-keyOp      PIC 9(1).
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
           PROCEDURE DIVISION  USING LK-keyOp,LK-Product.
           MAIN-PROCEDURE.
               PERFORM Recorrer
           EXIT PROGRAM.



      *============================================================*
      *> SECCION Leer-Producto
      *> Solicita el ID del producto y verifica si existe
      *============================================================*
       Iniciar SECTION.
           Recorrer.
               IF LK-keyOp = 1 THEN
                   DISPLAY "No existe el producto"
                   GOBACK
               ELSE
                   PERFORM Actualizar
                   DISPLAY "Actualizado correctamente"
                   GOBACK
               END-IF
           EXIT.

           Actualizar.
               DISPLAY "Ingrese Stock Actual actualizar: "
               ACCEPT   Stock
               DISPLAY "Ingrese Precio: "
               ACCEPT   Precio-Unitario
               ACCEPT  WS-Fecha FROM DATE
               MOVE    WS-Fecha(7:2) TO Dia-Modificacion
               MOVE    WS-Fecha(5:2) TO Mes-Modificacion
               MOVE    WS-Fecha(1:4) TO Ano-Modificacion
               ADD Ano-Modificacion TO WS-Year GIVING Ano-Modificacion
               DISPLAY  "Ingrese ubicacion: "
               ACCEPT   Ubicacion
               DISPLAY  "Ingrese Estado: "
               ACCEPT   Estado
           EXIT.
      *============================================================*
