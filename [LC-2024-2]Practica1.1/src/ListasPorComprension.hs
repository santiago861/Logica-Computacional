{-
- Integrantes:
- Islas Espino Julio Cesar
- López Espinoza Ashley Yael
- Reyes Medina Santiago Iván
-}

module ListasPorComprension where

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------

-- | EJERCICIO 1 | Conjunto potencia
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

-- | EJERCICIO 2 | Lista que regrese los elementos del 0 al n
elementsToN :: Int -> [Int]
elementsToN n = [0..n]

-- | EJERCICIO 3 | Lista que regrese los elementos pares hasta n
evenNumbersToN :: Int -> [Int]
evenNumbersToN n = [x | x <- [0..n], even x]

-- | EJERCICIO 4 | Lista que regrese los multiplos de n hasta k

-- | EJERCICIO 5 | Lista que regrese los elementos elevados al cuadrado hasta n

-- | EJERCICIO 6 | Lista que regrese la multiplicación de todos los elementos de una lista
con los de otra lista

-- | EJERCICIO 7 | 

-- | EJERCICIO 8 | 