--------------------------------------------------------------------------------
INTEGRANTES DEL EQUIPO
--------------------------------------------------------------------------------
- Islas Espino Julio Cesar
- López Espinoza Ashley Yael
- Reyes Medina Santiago Iván

--------------------------------------------------------------------------------
DESCRIPCIÓN DE IMPLEMENTACIÓN
--------------------------------------------------------------------------------
powerset: Esta función genera el conjunto potencia de una lista dada. Utiliza 
recursión para generar todos los subconjuntos posibles, tomando o no tomando cada 
elemento en la lista original.

elementsToN: Genera una lista que contiene los elementos desde 0 hasta el número 
dado n. Utiliza la notación de rango [0..n] para crear la lista de manera simple 
y directa.

evenNumbersToN: Crea una lista que contiene solo los números pares desde 0 hasta 
el número dado n. Utiliza una lista de comprensión con la condición even x para 
filtrar los números pares.

multiplesInRange: Genera una lista que contiene los múltiplos del primer 
argumento n hasta el número dado k. Utiliza una lista de comprensión con el rango 
[n, 2*n..k] para generar los múltiplos.

squaredElementsToN: Crea una lista que contiene los cuadrados de los números 
desde 0 hasta el número dado n. Utiliza una lista de comprensión con la expresión 
x^2 para elevar al cuadrado cada elemento.

multiplyLists: Devuelve una lista que contiene el resultado de multiplicar 
cada elemento de la primera lista con cada elemento de la segunda lista. Utiliza 
dos listas de comprensión anidadas para realizar todas las combinaciones de 
multiplicación.

generateTrees / enumerate: Estas funciones generan todos los árboles binarios 
posibles hasta una altura dada n. Utilizan la técnica de recursión para construir 
todos los árboles posibles.

listasLongitudN / concatenarListasPares: La primera función filtra una lista de 
listas para devolver solo aquellas sublistas que tienen una longitud igual a n. 
La segunda función concatena las sublistas pares de la lista dada. Ambas funciones 
utilizan listas de comprensión para realizar la filtración y concatenación 
respectivamente.