module Parcial1 where

isValidRoll :: [Int] -> Bool
isValidRoll xs
    | all (< 7) xs && all (>0) xs && (length xs) == 5 = True
    | otherwise = False

{-evalCond :: Bool -> Char -> Bool -> Bool
evalCond cond1 eval cond2
    | eval == '&' = cond1 && cond2
    | eval == '|' = cond1 || cond2
    | eval == '=' = cond1 == cond2
    | eval == '>' = aux cond1 cond2
    | otherwise = error "!"
        where
            aux x y
                | not x = True
                | x && y = True
                | otherwise = False
-}
sortPair :: (Double, Double) -> (Double, Double)
sortPair (a,b) = (min a b, max a b)

fun :: [a] -> [a] -> a
fun x y = head x

f :: [a] -> [a]
f (x:z:xs) = xs
f _ = []

evalCond :: Bool -> Char -> Bool -> Bool
evalCond x c y
  | c == '&' = x && y
  | c == '|' = x || y
  | c == '=' = (x && y) || (not x && not y)
  | c == '>' = (not x) || (x && y)
  | otherwise = error ("!")