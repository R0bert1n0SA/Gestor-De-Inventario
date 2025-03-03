       IDENTIFICATION DIVISION.
       PROGRAM-ID. Mostrar AS "Mostrar".
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
           01 LK-Parametros.
               05 P-ID              PIC X(10).
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

       PROCEDURE DIVISION USING LK-Parametros.
           MAIN-PROCEDURE.
               PERFORM Imprimir
           EXIT PROGRAM.


           Imprimir.
               DISPLAY "--------------------------------"
               DISPLAY "ID: " P-ID
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
               DISPLAY "--------------------------------"
           EXIT.
