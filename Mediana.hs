import Data.Char

module Mediana where

median :: Int -> Int -> Int -> Int

median x y z = if ((x<=y)&&(y<=z)) then y else if ((x<=z) && (z<=y)) then z else if  ((y<=x) && (x<=z)) then x else if (y<=z && z<=x) then z else if (z<=x && x<=y) then x else y

num_list = ['0', '1', '2', '3', '4', '5', '6', '7']

octalDigit :: Char -> Int

octalDigit x = if (elem x num_list) then read(x:"") else (-1)

squaresSum :: Int -> Int -> Int

squaresSum x y = if (x < y) then ((x*x) + (squaresSum (x+1) y) ) else 0

countLines:: String -> Int
countLines [] = 1
countLines ('\n' : xs) = 1 + countLines(xs)
countLines ('\\':('n' : xs)) = 1 + countLines(xs)
countLines (x:xs) = countLines(xs)

base8 :: Int -> String
base8 n | n < 0 = error "Ta mal"
base8 n | n < 8 = show n
base8 n = base8(div n 8) ++ show (mod n 8)



onlyLetters :: String -> String
onlyLetters [] = []
onlyLetters (x:xs) | isAlpha x = x ++ onlyLetters(xs)
onlyLetters (_:xs) = onlyLetters(xs)


--test