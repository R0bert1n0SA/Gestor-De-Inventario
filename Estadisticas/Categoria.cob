       IDENTIFICATION DIVISION.
       PROGRAM-ID. Categoria AS "Categoria".
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Productos
           ASSIGN TO 'Productos.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS Product-ID
               FILE STATUS IS Ps.

           SELECT Tcat
           ASSIGN TO 'Temporal'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS Cat-Name
               FILE STATUS IS Ps.

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

       FD Tcat.
       01 Temp-Reg.
           05 Cat-Name PIC X(20).
           05 cant PIC 9(9).

       WORKING-STORAGE SECTION.
           01 Ps       PIC XX.
           01 EOF-Flag PIC X(1) VALUE "N".
           01 EOF-FlagT PIC X(1) VALUE "N".
           01 TempCat  PIC X(30).
       LINKAGE SECTION.
           01 Flag PIC 9(2).
       PROCEDURE DIVISION USING Flag.
       MAIN-PROCEDURE.
           PERFORM Cantidad
       EXIT PROGRAM.




       CrearT.
           OPEN OUTPUT Tcat
           CLOSE Tcat
           EXIT.

       Contar.
           OPEN I-O Tcat
           MOVE TempCat TO Cat-Name
           READ Tcat INTO Temp-Reg KEY IS Cat-Name
               INVALID KEY
                    COMPUTE cant=(0 + 1)
                    WRITE Temp-Reg
                NOT INVALID KEY
                    COMPUTE cant=(cant + 1)
                    REWRITE Temp-Reg
           END-READ
           EXIT.


       Cantidad.
           OPEN INPUT Productos
           PERFORM CrearT
           PERFORM UNTIL EOF-Flag = 'Y'
               READ Productos INTO Product
                   AT END
                       MOVE 'Y' TO EOF-Flag
                   NOT AT END
                       MOVE Categoria TO TempCat
                       PERFORM Contar
               END-READ
           END-PERFORM
           CLOSE Productos
           EXIT.
