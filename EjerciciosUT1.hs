{-# LANGUAGE MultiWayIf #-}

module EjerciciosUT1 where

import Data.List

--nextMonth, previousMonth

rockPaperScissors :: Integer -> Integer -> Integer
rockPaperScissors x y 
    | (x == y) = 0
    | (x - y) == -1 || (x - y) == 2 = -1
    | (x - y) == 1 || (x - y) == -2 = 1
    | otherwise = error("Ingrese valores validos")


fromInchestoFeet :: Double -> Double
fromInchestoFeet x 
    | x >= 0 = x / 12
    | otherwise = error ("Ingrese un valor positivo")


fromYardstoFeet :: Double -> Double
fromYardstoFeet x
    | x >= 0 = x*3
    | otherwise = error ("Ingrese un valor positivo")


inRange :: Float -> Float -> Float -> Bool
inRange x y z
    | x<= z && y >= z = True
    |otherwise = False


fromDirChar :: Char -> Integer 
fromDirChar x
    | x == '>' = 1 
    | x == '<' = -1 
    | otherwise = 0


isSorted :: [Integer] -> Char -> Bool
isSorted x s
    | s == '>' = if
        | x == [] -> True
        | length x == 1 -> True
        | length x == 2  -> head x > head (tail x)
        | otherwise -> head x > head (tail x) && isSorted (tail x) s
    | s == '<' = if
        | x == [] -> True
        | length x == 1 -> True
        | length x == 2  -> head x < head (tail x)
        | otherwise -> head x < head (tail x) && isSorted (tail x) s 
    | otherwise = error("ingrese el tipo de lista correctamente")    


fromDirStr :: String -> Integer -> Integer
fromDirStr x y
    | x == [] = 0
    | (y > 0) = fromDirStr (tail x) (y-1)
    | otherwise = fromDirChar (head x) + fromDirStr (tail x) 0 -- entra cuando y es cero


toDirStr :: Integer -> String
toDirStr x
    | x == 0 = ""
    | x > 0 = '>' : toDirStr (x-1)
    | otherwise = '<' : toDirStr (x+1) 


traceDirStr :: String -> [Integer]
traceDirStr x
    | x == [] = []
    | last x == '<' || last x == '>' = traceDirStr (init x) ++ [fromDirStr x 0] 
    | otherwise = traceDirStr (init x)


isGenerala :: [Integer] -> Bool
isGenerala x
    | (length x == 5) && (head x >= 1) && (head x <= 6) && all(== head x) x = True
    | otherwise = False


isPoker :: [Integer] -> Bool
isPoker x
    | (length (filter (== head x) x) == 4 || length (filter (== last x) x) == 4 ) && length x == 5 && valoresPermitidos x = True
    | otherwise = False
       where
           valoresPermitidos x = length (filter (>6) x) == 0 && length (filter (<1) x) == 0



