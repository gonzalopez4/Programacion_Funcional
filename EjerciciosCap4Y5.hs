module EjerciciosCap4Y5 where

import Data.List

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage m n p = esMayorQueAverage m average + esMayorQueAverage n average + esMayorQueAverage p average
    where
        average = div (m+n+p) 3
        esMayorQueAverage x a
            | x > a = 1
            | otherwise = 0

pushRight :: String -> Int -> String
pushRight xs n
    | n == 0 = xs
    | (length xs) < n = ' ' :  pushRight xs (n-1)
    | (length xs) > n = error("Ingrese un numero mas grande!")
    | otherwise = head xs : pushRight (tail xs) (n-1)


nOf :: Int -> a -> [a]
nOf n x
    | n <= 0 = []
    | otherwise = x : nOf (n-1) x

minAfterMin :: [Int] -> Int
minAfterMin xs
    | xs == [] = error("Lista vacia!!")
    | elem (minimum xs + 1) xs == False = minimum xs + 1
    | otherwise = minAfterMin (delete (minimum xs) xs)

updateTriplet :: (a,a,a) -> Int -> a -> (a,a,a)
updateTriplet (a,b,c) n x
    | n == 1 = (x,b,c)
    | n == 2 = (a,x,c)
    | n == 3 = (a,b,x)
    | otherwise = (a,b,c)
