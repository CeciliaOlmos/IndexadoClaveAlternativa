      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT socios ASSIGN TO "..\arch.dat"
           ORGANIZATION INDEXED
           ACCESS MODE is DYNAMIC
           record KEY is soc-nro
           ALTERNATE record key is soc-nom WITH DUPLICATES.
       DATA DIVISION.
       FILE SECTION.
       fd  socios.
       01  soc-reg.
           03 soc-nro pic 999.
               88 no-quiere-mas value 0.
           03 soc-nom pic x(10).
           03 soc-apell pic x(10).
           03 soc-tel pic 9(10).
           03 soc-provincia pic x(12).
           03 soc-localidad pic x(12).
           03 soc-calle pic x(12).
           03 soc-nro-calle pic 9(4).
       WORKING-STORAGE SECTION.
       01  lin-soc.
           03 filler pic x(14) value "NUMERO SOCIO:".
           03 l-soc-num pic zzz value spaces.
           03 filler pic x(13) value all ".".
           03 filler pic x(11) value "NOMBRE:".
           03 l-nombre pic x(10) value spaces.
           03 filler pic x(23) value spaces.
       01  lin-resto.
           03 filler pic x(30) value all ".".
           03 filler pic x(9) value "APELLIDO:".
           03 filler pic x(2) value spaces.
           03 l-apell pic x(10) value spaces.
           03 filler pic x(21) value spaces.
       01  lin-tel.
           03 filler pic x(30) value all ".".
           03 filler pic x(9) value "TELEFONO:".
           03 filler pic x(2) value spaces.
           03 l-telef pic zzzzzzzzzz value spaces.
           03 filler pic x(21) value spaces.
       01  lin-provincia.
           03 filler pic x(30) value all ".".
           03 filler pic x(10) value "PROVINCIA:".
           03 filler pic x(1) value spaces.
           03 l-prov pic x(12) value spaces.


       01  lin-dom.
           03 filler pic x(30) value all ".".
           03 filler pic x(10) value "DOMICILIO:".
           03 l-loc pic x(10) value spaces.
           03 filler pic x(1) value spaces.
           03 l-calle pic x(12) value spaces.
           03 l-nro-calle pic zzz9 value spaces.
           03 filler pic x(18) value spaces.

       77  sen pic 9.
           88  fin-de-archivo value 1.
       77  nom-ant pic x(10).
       77  w-opcion pic 9.
           88 salir value 3.
       77  w-cont pic 9 value zero.
       77  w-i pic 99.
       77  soc-ant pic 999.
       01  tabla-socios.
           03 largo pic 99.
           03 vec-soc OCCURS 1 to 9999 DEPENDING on largo value zeros.
               05 vec-soc-nro pic 999.
               05 vec-soc-nombre pic x(10).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO-BUSQUEDA.
           PERFORM with test after UNTIL salir
               PERFORM 200-MENU
               PERFORM 300-PROCESO-MENU
           END-PERFORM.
           PERFORM 400-FIN-BUSQUEDA.
           STOP RUN.

       100-INICIO-BUSQUEDA.
           OPEN INPUT socios.

       200-MENU.
           PERFORM 230-MOSTRAR-OPCIONES.
           ACCEPT w-opcion.

       230-MOSTRAR-OPCIONES.
           display "1 - Buscar por número de socio".
           display "2 - Buscar por nombre de socio".
           display "3 - Salir".

       300-PROCESO-MENU.
           EVALUATE w-opcion
               WHEN 1 PERFORM 330-LEER-POR-NUM thru 330-F-LEER-POR-NUM
               WHEN 2 PERFORM 350-LEER-POR-NOM
               WHEN 3 NEXT SENTENCE
           END-EVALUATE.

       330-LEER-POR-NUM.
           PERFORM 360-PIDO-NUM.
           PERFORM 363-BUSCO-SOCIO.
           PERFORM 335-MOSTRAR-DATOS.
       363-BUSCO-SOCIO.
           READ SOCIOS INVALID KEY
                           display "Numero no encontrado"
                           GO TO 330-F-LEER-POR-NUM.
       330-F-LEER-POR-NUM.
           EXIT.

       335-MOSTRAR-DATOS.
           PERFORM 340-ARMAR-LINEA.
           DISPLAY lin-soc.
           PERFORM 345-mostrar-resto.

       345-mostrar-resto.
           DISPLAY lin-resto.
           DISPLAY lin-tel.
           DISPLAY lin-provincia.
           DISPLAY lin-dom.
       340-ARMAR-LINEA.
           MOVE soc-nro TO l-soc-num.
           MOVE soc-nom TO l-nombre.
           MOVE soc-apell TO l-apell.
           MOVE soc-tel TO l-telef.
           MOVE soc-provincia TO l-prov.
           MOVE soc-localidad TO l-loc.
           MOVE soc-calle to l-calle.
           MOVE soc-nro-calle to l-nro-calle.

       350-LEER-POR-NOM.
           move zeros to sen.
           PERFORM 355-PIDO-NOMBRE.
           move nom-ant to soc-nom.
           START SOCIOS KEY IS = soc-nom INVALID KEY
                           display "Nombre no encontrado"
                        NOT INVALID KEY
                           PERFORM 359-PROCESAR-SOCIO.

           355-PIDO-NOMBRE.
           display "Ingrese nombre".
           accept nom-ant.

       357-LEER-SOCIOS.
           READ SOCIOS NEXT AT END move 1 to sen.

       359-PROCESAR-SOCIO.
           PERFORM 357-LEER-SOCIOS
           PERFORM UNTIL fin-de-archivo OR soc-nom NOT = nom-ant
                   PERFORM 362-ARMAR-VECTOR
                   PERFORM 357-LEER-SOCIOS
      *             add 1 to w-cont
           END-PERFORM.
           PERFORM 370-ELEGIR-CODIGO.
       362-ARMAR-VECTOR.
           MOVE soc-nro TO vec-soc-nro(largo).
           MOVE soc-nom TO vec-soc-nombre(largo).
           ADD 1 TO largo.
           move soc-nro to soc-ant.

      *     DISPLAY "Numero socio: ",soc-nro," Nombre: ",soc-nom.

       360-PIDO-NUM.
           display "Ingrese un nro de socio"
           accept soc-nro.
       370-ELEGIR-CODIGO.
           IF largo = 1 THEN
                MOVE soc-ant to soc-nro
                PERFORM 363-BUSCO-SOCIO
                PERFORM 345-MOSTRAR-RESTO
           ELSE
               PERFORM VARYING w-i from 1 by 1 until w-i > largo
                  DISPLAY vec-soc(w-i)
               END-PERFORM
           END-IF.

       400-FIN-BUSQUEDA.
           CLOSE SOCIOS.

       END PROGRAM YOUR-PROGRAM-NAME.
