       IDENTIFICATION DIVISION.
       PROGRAM-ID. Estadisticas-Menu AS "Estadisticas-Menu".
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Productos ASSIGN TO 'Productos.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS Product-ID
               FILE STATUS IS WS-FileStatus.

           SELECT TCont ASSIGN TO 'Temporal-Cont'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TC-Categoria
               FILE STATUS IS WS-File-StatusTemp .

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
           05 P-Stock-Minimo         PIC 9(7) .
           05 P-Estado               PIC X(10).
           05 P-Descripcion          PIC X(100).
           05 P-Unidad-Medida        PIC X(2).

       FD TCont.
       01 Contador.
           05 TC-Categoria         PIC X(20).
           05 TC-Total             PIC 9(9).

       WORKING-STORAGE SECTION.
           01 WS-Control.
               05 WS-FileStatus        PIC XX.
               05 WS-File-StatusTemp   PIC XX.
               05 WS-flag              PIC 9(1) VALUE 0.
               05 WS-opcion            PIC S9(2).
               05 WS-Input             PIC X(3).

           01 WS-Recorrido.
               05 R-EOF                PIC X(1).
               05 R-estado             PIC X(1).



           01 WS-GeneralSubrutina.
               05 GS-Total             PIC 9(9).

           01 WS-ProductosSubrutina.
               05 PS-Top               PIC 9(7) VALUE 0.
               05 PS-NombreRank        PIC X(30).


           01 WS-CategoriaSubrutina.
               05 CS-Maximo                PIC 9(8).
               05 CS-CategoriaMax          PIC X(20).

           01 WS-FinanzasSubrutina.
               05 FS-Total             PIC 9(9)v99.
               05 FS-NombreF           PIC X(30).
               05 FS-Top               PIC 9(5)v99.


           01 WS-TiempoSubrutina.
               05 TS-DiasDesac         PIC 9(3).
               05 TS-Fecha-Reg.
                   10 RF-Anio           PIC 9(4).
                   10 RF-Mes            PIC 9(2).
                   10 RF-Dias           PIC 9(2).
               05 TS-Fecha-Mod.
                   10 MF-Anio           PIC 9(4).
                   10 MF-Mes            PIC 9(2).
                   10 MF-Dias           PIC 9(2).
               05 TS-DiasPorMes         PIC 9(2) OCCURS 13 TIMES.
               05 TS-FechaMasReciente   PIC 9(8) VALUE 0.
               05 TS-FechaString        PIC X(12).

           01 WS-tecla     PIC X(1).
           01 WS-Opcion11  PIC 9(1).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Estadisticas"
           PERFORM Bucle
       EXIT PROGRAM.

      *================================================================*
       *>  SECCION Verificar
       *>  Seccion donde se verifica todo lo nesesario para el buen
       *>  funcionamiento
      *================================================================*

       Verificar SECTION.
           *> Se Verifica q la opcion sea un numero
           Verificar-Input.
               IF FUNCTION TEST-NUMVAL(WS-Input) = 0 THEN
                   MOVE FUNCTION NUMVAL(WS-Input) TO WS-opcion
               ELSE
                   MOVE -99 TO WS-opcion
               END-IF
           EXIT.

           *> Control de errores de archivo contador de categorias
           Verificar-Categorias.
               OPEN INPUT TCont
               IF WS-File-StatusTemp = '00' THEN
                   MOVE 'C' TO R-estado
                   MOVE 'N' TO R-estado
                   PERFORM Recorrer-Generico
                   CLOSE TCont
                   DELETE FILE TCont
               ELSE
                   CALL "Errores" USING WS-File-StatusTemp
               END-IF
           EXIT.

           *> Control de errores de archivo Productos
           Verificar-Productos.
               OPEN INPUT Productos
               IF WS-FileStatus  = '00' THEN
                   PERFORM Inicio
                   PERFORM Recorrer-Generico
                   CLOSE Productos
                   PERFORM Resultado
               ELSE
                   CALL "Errores" USING WS-FileStatus
               END-IF
           EXIT.

          *> Verifica si creo el Archivo contador dependiendo opcion
          *> y muestra un encabezado dependiendo opcion
           Verificar-Creacion-Temp.
               IF WS-opcion = 6 OR WS-opcion = 7 THEN
                   PERFORM CrearTC
               END-IF
           EXIT.
      *================================================================*


      *================================================================*
       *>  SECCION Iniciar
       *>  Seccion donde se Inicializa todo la logica del programa
      *================================================================*

       Inicializar SECTION.
           Inicio.
            *>Inicializar Registros
               EVALUATE WS-opcion
                   WHEN 1 THRU 2
                       PERFORM Iniciar-Gs
                   WHEN 3 THRU 5
                       PERFORM Iniciar-Ps
                   WHEN 6 THRU 7
                       PERFORM Iniciar-Cs
                   WHEN 8 THRU 10
                       PERFORM Iniciar-Fs
                   WHEN 11 THRU 12
                       PERFORM Iniciar-Ts
               END-EVALUATE
               MOVE 0       TO WS-Opcion11
               MOVE 'N'     TO R-EOF
               MOVE 'P'     TO R-estado
           EXIT.


           Iniciar-Gs.
               MOVE 0 TO GS-Total
           EXIT.


           Iniciar-Ps.
               MOVE " " TO PS-NombreRank
               IF WS-opcion = 3
                   MOVE 0   TO PS-Top
               ELSE
                   MOVE 9999999 TO PS-Top
               END-IF
           EXIT.


           Iniciar-Cs.
               MOVE 0 TO CS-Maximo
               MOVE " " TO CS-CategoriaMax
           EXIT.

           Iniciar-Fs.
               MOVE 0   TO FS-Total
               MOVE " " TO FS-NombreF
              IF WS-opcion = 9 THEN
                 MOVE 0 TO FS-Top
              ELSE
                 MOVE 99999 TO FS-Top
              END-IF
           EXIT.

           Iniciar-Ts.
               MOVE 0   TO TS-DiasDesac,TS-FechaMasReciente
               MOVE " " TO TS-FechaString
               PERFORM InicializarDiasPorMes
               INITIALIZE TS-Fecha-Reg.
               INITIALIZE TS-Fecha-Mod.
           EXIT.



           InicializarDiasPorMes.
               MOVE 31 TO  TS-DiasPorMes(1),
                           TS-DiasPorMes(3),
                           TS-DiasPorMes(5),
                           TS-DiasPorMes(7),
                           TS-DiasPorMes(8),
                           TS-DiasPorMes(10),
                           TS-DiasPorMes(12)

               MOVE 30 TO  TS-DiasPorMes(4),
                           TS-DiasPorMes(6),
                           TS-DiasPorMes(9),
                           TS-DiasPorMes(11)

               MOVE 28 TO  TS-DiasPorMes(2)
               MOVE 29 TO  TS-DiasPorMes(13)
           EXIT.


           CrearTC.
               OPEN INPUT TCont
               IF WS-File-StatusTemp = "35" THEN
                   OPEN OUTPUT TCont
                   CLOSE TCont
               ELSE
               CLOSE TCont
               END-IF
           EXIT.

           Carga-Elemento.
               MOVE P-Fecha-Registro     TO TS-Fecha-Reg
               MOVE P-Fecha-Modificacion TO TS-Fecha-Mod
           EXIT.

      *================================================================*


      *================================================================*
       *>  SECCION Ciclo-General
       *>  Seccion de todas la funciones del menu
      *================================================================*
       Ciclo SECTION.
           Bucle.
               PERFORM UNTIL WS-flag = 1
                   PERFORM Menu
                   DISPLAY X"1B" & "[2J"
                   PERFORM Verificar-Productos
                   PERFORM Continuar
                   DISPLAY X"1B" & "[2J"
               END-PERFORM
           EXIT.

           Menu.
               DISPLAY "------------Estadisticas----------------"
               DISPLAY "1.Total de productos registrados"
               DISPLAY "2.Stock total General"
               DISPLAY "3.Producto con mayor stock"
               DISPLAY "4.Producto con menor stock"
               DISPLAY "5.Productos bajo stock minimo"
               DISPLAY "6.Cantidad de productos por categoria "
               DISPLAY "7.Categoria con mas productos registrados"
               DISPLAY "8.Valor total del inventario "
               DISPLAY "9.Producto mas caro"
               DISPLAY "10.Producto mas barato"
               DISPLAY "11.Productos sin actualizacion reciente"
               DISPLAY "12.Fecha del ultimo registro"
                           " aniadido/modificado"
               DISPLAY "0. Menu principal"
               DISPLAY "Ingrese una Opcion: "
               ACCEPT WS-Input
               PERFORM Verificar-Input
               PERFORM Verificar-Creacion-Temp
           EXIT.


           Continuar.
               IF WS-opcion = 0 THEN
                   GOBACK
               ELSE
                   DISPLAY "Presione Enter para continuar"
                   ACCEPT WS-tecla
               END-IF
           EXIT.




      *>---------------------------------------------------------------

      *>---------------------------------------------------------------
       Recorrer SECTION.
           Recorrer-Generico.
               IF WS-opcion = 5
                   DISPLAY "---------Productos con Bajo Stock--"
                   "----------"
               END-IF
               IF WS-opcion = 11 THEN
                   PERFORM Opcion_11
               END-IF
               PERFORM UNTIL R-EOF = 'Y'
                   IF R-estado = 'P' THEN
                       PERFORM Productos-Lectura
                   ELSE
                       PERFORM Categoria-Lectura
                   END-IF
               END-PERFORM
               MOVE 'N' TO R-EOF
           EXIT.

           Productos-Lectura.
               READ Productos INTO Product
                   AT END
                       MOVE 'Y' TO R-EOF
                   NOT AT END
                       DISPLAY "3"
                       PERFORM Carga-Elemento
                       PERFORM Evaluar-Opciones
                END-READ
           EXIT.

           Categoria-Lectura.
               READ TCont INTO Contador
                   AT END
                       MOVE 'Y' TO R-EOF
                   NOT AT END
                       PERFORM Evaluar-Categoria
                END-READ
           EXIT.

           Evaluar-Categoria.
               EVALUATE WS-opcion
                   WHEN 6
                       DISPLAY TC-Categoria ": "TC-Total
                   WHEN 7
                       IF TC-Total > CS-Maximo THEN
                           MOVE TC-Total TO CS-Maximo
                           MOVE TC-Categoria TO CS-CategoriaMax
                       END-IF
               END-EVALUATE
           EXIT.


           Evaluar-Opciones.
               EVALUATE WS-opcion
                   WHEN 1 THRU 2
                       CALL "General" USING WS-opcion,
                       WS-GeneralSubrutina,P-Stock
                   WHEN 3 THRU 5
                       CALL "Productos" USING WS-opcion,P-Stock,P-Nombre
                       ,WS-ProductosSubrutina,P-Stock-Minimo
                   WHEN 6 THRU 7
                       CALL "Categoria" USING WS-opcion,P-Categoria
                   WHEN 8 THRU 10
                       CALL "Finanzas" USING WS-opcion,P-Stock,P-Nombre
                       ,P-Precio-Unitario,WS-FinanzasSubrutina
                   WHEN 11 THRU 12
                       CALL "Tiempo" USING WS-opcion,P-Nombre
                       ,WS-TiempoSubrutina
                   WHEN 0
                       DISPLAY X"1B" & "[2J"
                       MOVE 1 TO WS-flag
                   WHEN OTHER
                       DISPLAY "ERROR caracter no valido"
               END-EVALUATE
           EXIT.


           Resultado.
               EVALUATE WS-opcion
                   WHEN 1
                       DISPLAY "Productos Registrados: "GS-Total
                   WHEN 2
                       DISPLAY "Stock total General: " GS-Total
                   WHEN 3
                       DISPLAY "Producto con mas stock: " PS-NombreRank
                   WHEN 4
                       DISPLAY "Producto con menor Stock: "PS-NombreRank
                   WHEN 6 THRU 7
                       PERFORM Verificar-Categorias
                       IF WS-opcion = 7 THEN
                           DISPLAY CS-CategoriaMax ": "CS-Maximo
                       END-IF
                   WHEN 8
                       DISPLAY "Costo Total de Inventario: "FS-Total
                   WHEN 9
                       DISPLAY "EL Producto Mas Caro es: " FS-NombreF
                      " Precio: "FS-Top
                   WHEN 10
                       DISPLAY "EL Producto Mas Barato es: "
                       FS-NombreF" Precio: "FS-Top
                   WHEN 12
                       DISPLAY "Fecha del ultimo registro: "
                       TS-FechaString
               END-EVALUATE
           EXIT.

           Opcion_11.
               PERFORM UNTIL WS-Opcion11 = 1
                   DISPLAY "Cuantos dias se considera no"
                   "actualizado recientemente maximo 300: "
                   ACCEPT WS-Input
                   IF  FUNCTION TEST-NUMVAL(WS-Input) = 0 AND
                   (WS-Input >= 1 and WS-Input <= 300) THEN
                       MOVE WS-Input TO TS-DiasDesac
                       MOVE " " TO WS-Input
                       MOVE 1 TO WS-Opcion11
                   ELSE
                       DISPLAY "Valor no valido "
                       IF FUNCTION TEST-NUMVAL(WS-INPUT) = 1 THEN
                           DISPLAY "No es un numero"
                       ELSE
                           DISPLAY "Fuera de Rango [1...300]"
                       END-IF
                   END-IF
               END-PERFORM
           EXIT.

      *>---------------------------------------------------------------
