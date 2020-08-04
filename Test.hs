module Test where
{-
roundP :: Double -> Double -> Double

roundP x y = fromInteger(round (x*(10**y)))/(10**y)

distintos :: [Int]-> [Int]
distintos [] = []
distintos xs = head xs : (distintos(filter (/= head xs) xs))

isEscalera :: [Int] -> Bool
isEscalera xs
  | xs == [1,2,3,4,5] || xs == [2,3,4,5,6] || xs == [3,4,5,6,1] = True
  | otherwise = False

isFull :: [Int] -> Bool
isFull xs
  | xs == [] = False
  | length xs == 5 && length(filter (==head xs)xs) == 2 && length (filter(== (head filtro))  filtro) == 3 = True
  | length xs == 5 && length(filter (==head xs)xs) == 3 && length (filter(== (head filtro))  filtro) == 2 = True
  | otherwise = False
    where
      filtro = filter (/=head xs) xs


type Naipe = (Int, Int)

esPieza :: Naipe -> Naipe -> Bool
esPieza (a,b) (c,d) = a==c && pieza d
  where
    pieza x = elem x [2,4,5,10,11]

isValidRoll :: [Int] -> Bool
isValidRoll xs
    | all (< 7 && >0) xs && (length xs) == 5 = True
    | otherwise = False
-}
--modificacion

sumK :: Int -> Int -> [[Int]]
sumK 0 0 = [[]]
sumK 0 k | k > 0 = [0:y | y <- sumK 0 (k - 1)]
sumK n k | n > 0 && k > 0 = [x:y | x <- [0..n], y <- sumK (n - x) (k - 1)]
sumK _ _ = []