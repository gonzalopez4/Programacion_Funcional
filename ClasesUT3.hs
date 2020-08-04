module ClasesUT3 where

import Data.List

--Toma una lista de Doubles, retorna otra lista con los valores de la lista dada que son enteros.
integers :: [Double] -> [Double]
integers = filter isInteger
    where isInteger n = n == (fromIntegral (floor n))

--Toma una lista de Doubles, y retorna otra lista con los valores de la lista dada que son enteros, convertidos a enteros.
integers2 :: [Double] -> [Int]
integers2 xs = map floor (integers xs)

--Toma una lista de numeros enteros y restringe su valor a un rango dado por otros dos numeros enteros.
clampList :: Int -> Int -> [Int] -> [Int]
clampList x y [] = []
clampList x y xs
    | x <= y = map f xs
    | otherwise = error "!"
     where
        f z = if (z <= y && z >= x) then z else if z <= x then x else y

--Análogas a map pero para tuplas de dos y tres elementos respectivamente.
map2 :: (a -> b) -> (a,a) -> (b,b)
map2 p (a,b) = (p a , p b)

map3 :: (a -> b) -> (a,a,a) -> (b,b,b)
map3 p (a,b,c) = (p a , p b, p c)

{- Toma una funcion f, un entero n, y una lista xs.
 La lista xs se parte en sublistas solapadas de largo n.
 El resultado es la lista de los resultados de llamar a f con cada sublista -}
windowMap :: ([a] -> b) -> Int -> [a] -> [b]
windowMap f n xs
    | n > length xs || length xs == 0 = []
    | length xs >= n && n>0 =  f (take n xs) : windowMap f n (tail xs)
    | otherwise = error "!"

-- Toma dos listas de Double(representando vectores), y calcula la distancia entre ambas --
euclideanDistance :: [Double] -> [Double] -> Double
euclideanDistance xs ys
    | length xs == length ys = sqrt(resultado xs ys)
    | otherwise = error "!"
        where
            resultado xs ys
                | length xs == 1 = (head xs - head ys)^2
                | length xs > 1 = (head xs - head ys)^2 + resultado (tail xs) (tail ys)
                | otherwise = error "!"

-- Dadas dos listas, calcula la cantidad de valores diferentes posicion a posicion --
hammingDistance :: (Eq a) => [a] -> [a] -> Int
hammingDistance xs ys
    | xs == [] && ys == [] = 0
    | xs == [] =  1 + hammingDistance xs (tail ys)
    | ys == [] =  1 + hammingDistance (tail xs) ys
    | head xs == head ys = hammingDistance (tail xs) (tail ys)
    | otherwise = 1 + hammingDistance (tail xs) (tail ys)

-- Dadas dos listas de numeros, calcula la suma de los valores absolutos de las diferencias a pares
manhattanDistance :: [Double] -> [Double] -> Double
manhattanDistance xs ys
    | length xs == length ys = sum (zipWith zipFun xs ys)
    | otherwise = 1.0/0.0
        where zipFun x y = abs (x-y)

type Point = [Double]
type Points = [Point]
type DistanceFunction = Point -> Point -> Double

nearest :: DistanceFunction -> Point -> Points -> Point
nearest df x xs = n
    where (_, n) = minimum [(df x y, y) | y <- xs]

knn :: DistanceFunction -> Int -> Point -> Points -> Points
knn df n x xs = map snd (take n sorted)
    where sorted = sort [(df x y, y) | y <- xs]

{-Dadas dos listas de numeros, calcula la raiz cuadrada del promedio
de los cuadrados de las diferencias de a pares -}
rmsd :: [Double] -> [Double] -> Double
rmsd xs ys
    | xs == [] && ys == [] = 0
    | length xs /= length ys = error "!"
    | otherwise = sqrt ( ((head xs - head ys) ^ 2)/2 ) + rmsd (tail xs) (tail ys)