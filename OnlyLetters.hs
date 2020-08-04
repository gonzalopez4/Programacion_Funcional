module OnlyLetters where

import Data.Char

onlyLetters :: String -> String
onlyLetters [] = []
onlyLetters (x:xs) | isAlpha x = x : onlyLetters(xs)
onlyLetters (_:xs) = onlyLetters(xs)

clampList :: Int -> Int -> [Int] -> [Int]
clampList x y [] = []
clampList x y (n:ns)
    | x > y = error "Esta mal"
    | n <= x = (x:clampList x y ns)
    | n >= y = (y:clampList x y ns)
    | otherwise = (n:clampList x y ns)

startsWith :: (Eq a) => [a] -> a -> Bool
startsWith x y
    | x == [] = False
    | otherwise = head x == y

setPairFst :: (a,b) -> a -> (a,b)
setPairFst (x,y) n = (n,y)

setPairSnd :: (a,b) -> b -> (a,b)
setPairSnd (x,y) n = (x,n)

setElemAt :: [a] -> a -> Int -> [a]
setElemAt [] x n
    | n ==0 = [x]
    | otherwise = error "!"
setElemAt xs x n
    | n == 0 = x : tail xs
    | n < (length xs) = head xs : setElemAt (tail xs) x (n-1)
    | otherwise = error "!"

setElemsAt :: [a] -> [a] -> Int -> [a]
setElemsAt [] x n
    | n ==0 = x
    | otherwise = error "!"
setElemsAt xs x n
    | n== 0 = x ++ xs
    | n > 0 && n <= (length xs) = take n xs ++ x ++ drop n xs
    | otherwise = error "!"