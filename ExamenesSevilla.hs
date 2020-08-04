module ExamenesSevilla where

import Data.List

-- Examen 1 (11 de Noviembre 2019)

-- 1234 -> [1,2,3,4]
digits :: Int -> [Int]
digits 0 = []
digits x = digits (x `div`10) ++ [ x `mod` 10]

de3en3:: Int -> [[Int]]
de3en3 xs
    | length (digits xs) < 3 = []
    | otherwise = grupoDe3 (digits xs)
    where
        grupoDe3 x
            | length x < 3 = []
            | otherwise =  [take 3 x]  ++ grupoDe3 (tail x)

esEndiablado:: Int -> Bool
esEndiablado x = [6,6,6] `elem` de3en3 (2*x)

entreMedias :: [Float] -> [Float]
entreMedias [] = []
entreMedias [x] = [x]
entreMedias (x:y:xs) = x: media x y : entreMedias (y:xs)
    where media x y = (x+y)/2

-- Examen 1 (7 de Noviembre 2018)
{-isPrime :: Int -> Bool
isPrime x
    | x > 1 && length [y | y <- [2..sqrt x],  x `mod` y == 0] == 0 = True
    | otherwise = False
    -}

isPrime :: Int -> Bool
isPrime x = length [d | d <- [1..x], x `mod` d == 0] == 2

divisoresPrimos :: Int -> [Int]
divisoresPrimos x = [y | y <- [2..x] , x `mod` y == 0, isPrime y]

esPotencia:: Int -> Int -> Bool
esPotencia x a = x `elem` [a^n | n <- [0..x]]
