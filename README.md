# LP-Decision-Trees

Project name: Decision Tree

Descripción:
		Este programa genera un decision tree a partir de los datos que recibe por la entrada ye se encarga de mostrárselo al usuario. Una vez generado el arbol el usuario podrá interactuar con el sistema mediante la consola.
		Entonces por cada atributo de agaricus-lepiota.data que envía el usuario, el sistema intentara hacer las predicciones para detectar de la clase del Mushroom. 

Installation: 
		1. Acceder a partir de la consola al directorio donde está el programa dts.hs
		2. Escribir el comando ghc dts.hs para compilar y generar el ejecutable del programa 

Usage:
		1. Hacer click al ejecutable dts.exe o escribir dts.exe en la consola.
		2. El programa mostrará el árbol que corresponde a la entrada (agaricus-lepiota.data)
		3. El programa pedirá al usuario que introduzca un atributo para hacer las predicciones (los atributos solo pueden los que están en el fichero agaricus-lepiota.data. Ejemplo
		   para la columna cape-shape si el usuario desea introducir el tipo conical solo tiene que introducir "conical"
		4. Si el atributo introducido por el usuario se le puede hacer la predicción de forma inmediata el programa devolverá el resultado. y vuelve a empezar desde el principio ( punto 3 )
		5. Si el atributo introducido por el usuario no se le puede hacer la predicción de forma inmediata El programa pedirá al usuario introducir un tipo de una columna especifica
		6. A partir de aquí se repite o el punto 4 o el punto 5 depende de la entrada del usuario.


Ejemplo de la interaccion :
				1. Caso inmediato:
						    <system> Which odor?
						    <user> pungent
						    <system> result: poisonous

				2. Caso no inmediato :
						    <system> Which odor?
						    <user> none
						    <system> Which spore-print-color
						    <user> white
						    <system> Which cap-color
						    <user> buff
						    <system> result: edible 

		Entonces el sistema después de generar la predicción volverá a preguntar which odor.


-------------------------------------------------------------------------------------------------------------------------------------
En los comentarios del codigo cuando digo un tipo de una columna. Se refiere as esto : c es un tipo de la columna cap-shape.
