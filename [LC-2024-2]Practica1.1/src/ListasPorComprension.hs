{-
- Integrantes:
- Islas Espino Julio Cesar
- López Espinoza Ashley Yael
- Reyes Medina Santiago Iván
-}

module ListasPorComprension where

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

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
multiplesInRange :: Int -> Int -> [Int]
multiplesInRange n k = [x | x <- [n, 2*n..k]]

-- | EJERCICIO 5 | Lista que regrese los elementos elevados al cuadrado hasta n
squaredElementsToN :: Int -> [Int]
squaredElementsToN n = [x^2 | x <- [0..n]]

-- | EJERCICIO 6 | Lista que regrese la multiplicación de todos los elementos de una lista con los de otra lista
multiplyLists :: [Int] -> [Int] -> [Int]
multiplyLists xs ys = [x * y | x <- xs, y <- ys]

-- | EJERCICIO 7 | 
-- Función para generar todos los árboles binarios posibles con una altura dada
generateTrees :: Int -> [Tree Int]
generateTrees 0 = [Leaf 1, Leaf 2] -- Árboles de altura 0
generateTrees n = concat [generateTrees' h | h <- [0..n-1]]
    where
        generateTrees' 0 = [Leaf 1, Leaf 2]
        generateTrees' h = [Node left right | lh <- [0..h-1], left <- generateTrees' lh, right <- generateTrees' (h-1-lh)]

-- Función principal para generar todos los árboles binarios hasta una altura n
enumerate :: Int -> [Tree Int]
enumerate n = generateTrees n

-- | EJERCICIO 8 | 
-- Crear lista de comprension que contenga a todas las listas de list de tamaño n 
listasLongitudN :: Int -> [[Int]] -> [[Int]]
listasLongitudN n list = [l | l <- list, length l == n]

concatenarListasPares :: [[Int]] -> [[Int]]
concatenarListasPares list = [l ++ l | l <- list, even (length l)]


--------------------------------------------------------------------------------
--------                            PRUEBAS                           --------
--------------------------------------------------------------------------------

-- Prueba para el ejercicio 1: Conjunto potencia
testPowerset :: Bool
testPowerset = powerset [1,2,3] == [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

-- Prueba para el ejercicio 2: Lista que regrese los elementos del 0 al n
testElementsToN :: Bool
testElementsToN = elementsToN 5 == [0,1,2,3,4,5]

-- Prueba para el ejercicio 3: Lista que regrese los elementos pares hasta n
testEvenNumbersToN :: Bool
testEvenNumbersToN = evenNumbersToN 10 == [0,2,4,6,8,10]

-- Prueba para el ejercicio 4: Lista que regrese los múltiplos de n hasta k
testMultiplesInRange :: Bool
testMultiplesInRange = multiplesInRange 3 20 == [3,6,9,12,15,18]

-- Prueba para el ejercicio 5: Lista que regrese los elementos elevados al cuadrado hasta n
testSquaredElementsToN :: Bool
testSquaredElementsToN = squaredElementsToN 4 == [0,1,4,9,16]

-- Prueba para el ejercicio 6: Lista que regrese la multiplicación de todos los elementos de una lista con los de otra lista
testMultiplyLists :: Bool
testMultiplyLists = multiplyLists [1,2,3] [4,5,6] == [4,5,6,8,10,12,12,15,18]

-- Prueba para el ejercicio 7: Generar árboles binarios hasta una altura dada
testEnumerate :: Bool
testEnumerate = length (enumerate 0) == 1 &&
                length (enumerate 1) == 3 &&
                length (enumerate 2) == 19 &&
                length (enumerate 3) == 723
                
-- Prueba para el ejercicio 8: Lista de comprensión que contenga a todas las listas de list de tamaño n
testListasLongitudN :: Bool
testListasLongitudN = listasLongitudN 2 [[1],[1,2],[1,2,3],[1,2,3,4]] == [[1,2]]

-- Prueba para el ejercicio 8: Concatenar listas pares consigo mismas
testConcatenarListasPares :: Bool
testConcatenarListasPares = concatenarListasPares [[1],[1,2],[1,2,3],[1,2,3,4]] == [[1,2,1,2],[1,2,3,4,1,2,3,4]]

-- Función principal para ejecutar todas las pruebas
runTests :: IO ()
runTests = do
    putStrLn "Prueba para el ejercicio 1: Conjunto potencia"
    print testPowerset

    putStrLn "Prueba para el ejercicio 2: Lista que regrese los elementos del 0 al n"
    print testElementsToN

    putStrLn "Prueba para el ejercicio 3: Lista que regrese los elementos pares hasta n"
    print testEvenNumbersToN

    putStrLn "Prueba para el ejercicio 4: Lista que regrese los múltiplos de n hasta k"
    print testMultiplesInRange

    putStrLn "Prueba para el ejercicio 5: Lista que regrese los elementos elevados al cuadrado hasta n"
    print testSquaredElementsToN

    putStrLn "Prueba para el ejercicio 6: Lista que regrese la multiplicación de todos los elementos de una lista con los de otra lista"
    print testMultiplyLists

    putStrLn "Prueba para el ejercicio 7: Generar árboles binarios hasta una altura dada"
    print testEnumerate

    putStrLn "Prueba para el ejercicio 8: Lista de comprensión que contenga a todas las listas de list de tamaño n"
    print testListasLongitudN

    putStrLn "Prueba para el ejercicio 8: Concatenar listas pares consigo mismas"
    print testConcatenarListasPares
