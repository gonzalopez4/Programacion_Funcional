module Rosenbrock where

import Data.List (minimumBy)

rosenbrock:: [Double] -> Double
rosenbrock (x:xs@(y:_)) = n + (rosenbrock xs)
    where n = 100 * ((y-x*x)^2) + (1-x)^2
rosenbrock _ = 0

delta:: [Double] -> Int -> Double -> [Double]
delta (list@(h:t)) pos n
    | pos == 0 = (h + n) : t
    | pos > 0 && pos < length list = h : delta (t) (pos - 1) n
    | otherwise = error ("!")

surroundings :: [Double] -> Double -> [[Double]]
surroundings (lista@(h:t)) n = [delta lista x d | x <- [0..(length lista)-1], d <- [n,(-n)]]

minRosenbrock :: [[Double]] -> (Double, [Double])
minRosenbrock list = (minimum [(rosenbrock x, x) | x <- list])

localMinimum :: [Double] -> Double -> Bool
localMinimum list n = list == snd (minRosenbrock(list: surroundings list n))

minimize :: [Double] -> Double -> [[Double]]
minimize list n = if (localMinimum list n) then [list] else list : minimize (snd (minRosenbrock(surroundings list n))) n


data Optimization = Minimize | Maximize | Approximate Double
    deriving (Show, Eq)


optimum:: (Ord a) => Optimization -> (a -> Double) -> [a] -> a
optimum opt f vs = snd (optimum1 opt f vs)


optimum1:: (Ord a) => Optimization -> (a -> Double) -> [a] -> (Double, a)
optimum1 Minimize f vs = minimum [(f v, v) | v <- vs]
optimum1 Maximize f vs = maximum [(f v, v) | v <- vs]
optCompare (Approximate t) f vs = resultado (minimum[( abs((f v) - t), f v, v) | v <- vs])
    where
        resultado (a, b, c)= (b, c)

{-
optimum1 opt f vs = minimumBy (optCompare opt) evaluated
    where evaluated = [(f v, v) | v <- vs]

optCompare Minimize (e1, _) (e2, _) = compare e1 e2
optCompare Maximize (e1, _) (e2, _) = compare e2 e1
optCompare (Approximate t) (e1, _ ) (e2, _) = compare (abs (e1 - t)) (abs (e2 - t))
-}
type Vector = [Double]
type EvalFunction = Vector -> Double

localOptimum:: Optimization -> EvalFunction -> Double -> Vector -> Bool
localOptimum opt f d v = v == optV
    where optV = optimum opt f (v:(surroundings v d))


hillClimbing:: Optimization -> EvalFunction -> Double -> Vector -> [(Double, Vector)]
hillClimbing opt f d v
    | v == v2 = [(n, v2)]
    | otherwise = ((f v),v):(hillClimbing opt f d v2)
    where (n, v2) = optimum1 opt f (v:(surroundings v d))