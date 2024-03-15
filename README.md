Este proyecto tiene como objetivo transferir datos de empleados desde un archivo secuencial a un archivo VSAM mediante un programa en COBOL.
Posteriormente, se podrán realizar operaciones como agregar, modificar y eliminar empleados en el archivo VSAM.
---------------------------------------------------------------------------------------------------------------------------------------------------

                                                          Pasos a seguir:

Migración de Datos:
Utilice un programa en COBOL para leer el archivo secuencial que contiene los datos de empleados, y transfiera esta información a un archivo VSAM.
La clave del archivo VSAM se basará en el número de legajo del empleado (LEGAJO).

Operaciones sobre el Archivo VSAM:
Desarrolle funciones en COBOL para agregar, modificar y eliminar empleados en el archivo VSAM.
Estas funciones permitirán la actualización dinámica de la información de los empleados.

Gestión de Ventas:
Considere que las ventas de cada empleado se almacenan en otro archivo secuencial.
Desarrolle procedimientos en COBOL para leer el archivo de ventas y vincular las ventas a los empleados correspondientes en el archivo VSAM.

--------------------------------------------------------------------------------------------------------------------------------------------------


                                                            Generación de Reportes:

Reporte Individual:
Cree un programa en COBOL para generar un informe detallado de un empleado seleccionado, incluyendo todas sus ventas.
<br />
Reporte de Ventas y Comisiones:
Desarrolle otro programa en COBOL para generar un informe que muestre la cantidad y el total de ventas por cada empleado.
Calcule una comisión del 13% sobre el total de las ventas.


-------------------------------------------------------------------------------------------------------------------------------------------------
                                                            Indice de programas:
<br />                                                            
PROGRAMA PVADDREG: Se encarga de transferir los registros de un archivo secuencial de entrada a un archivo VSAM.
<br />
PROGRAMA PVDELREG: Se encarga de eliminar los registros que coincidan con los legajos ingresados en el archivo secuencial.
<br />
PROGRAMA PVMODREG: Se encarga de modificar los registros que hay guardados en el archivo VSAM con los datos que se encuentran en el archivo secuencial, dependiendo de su legajo.
<br />
PROGRAMA PVINFVTA: Genera un informe de venta del usuario ingresado por SYSIN en la ejecucion del programa cobol.
<br />
PROGRAMA PVCOMVEN: Genera un detalle general de las ventas que realizo cada empleado y cuando comision va obtener segun sus ventas.
<br />
PROGRAMA VERFECH: Sub programa que se ocupa de detectar si una fecha ingresada es valida o no, en formato: DDMMYYYY.
<br />
![Bisiesto](https://github.com/LeandroTroncoso98/Proyecto-vendedores-VSAM/assets/105368488/66141163-9e59-414e-8cb6-33e707819850)
