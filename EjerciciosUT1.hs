{-# LANGUAGE MultiWayIf #-}

module EjerciciosUT1 where


nextMonth :: Integer -> Integer -> Integer

nextMonth n m
    | n >= 0 && n < 11 = if
        | m >= 0 -> n + (mod m 12)
        | otherwise -> previousMonth n ((-1)*m)
    | n == 11 = if
        | m == 0 -> 11
        | m > 0 -> mod (m-1) 12
        | otherwise -> previousMonth n ((-1)*m)
    | otherwise = error("Fuera de rango")


previousMonth :: Integer -> Integer -> Integer
previousMonth n m
    | n > 0 && n <= 11 = if
        | m >= 0 -> mod (n - (mod m 12)  + 12) 12
        | otherwise -> nextMonth n ((-1)*m)
    | n == 0 = if
        | m == 0 -> 0
        | m > 0 -> mod (12-m) 12
        | otherwise -> nextMonth n ((-1)*m)
    | otherwise = error("Fuera de rango")


rockPaperScissors :: Integer -> Integer -> Integer
rockPaperScissors x y
    | x-y > 1 = y-x+1
    | x-y < (-1) = y-x-1
    | otherwise = x-y

fromInchestoFeet :: Float -> Float
fromInchestoFeet x
    | x >= 0 = x / 12
    | otherwise = error ("ingrese un valor positivo")

fromYardstoFeet :: Float -> Float
fromYardstoFeet x
    | x >= 0 = x*3
    | otherwise = error ("ingrese un valor positivo")

inRange :: Float -> Float -> Float -> Bool
inRange x y z
    | x <= z && y >= z = True
    | otherwise = False

fromDirChar :: Char -> Integer
fromDirChar x
    | x == '>' = 1
    | x == '<' = (-1)
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
    | y > 0 = fromDirStr (tail x) (y-1)
    | otherwise = fromDirChar (head x) +  fromDirStr (tail x) 0

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
    | length x == 5 && all (== head x) x && head x > 0 && head x < 7 = True
    | otherwise = False

isPoker :: [Integer] -> Bool
isPoker x
    | (length (filter (== head x) x) == 4 || length (filter (== last x) x) == 4 ) && length x == 5 && valoresPermitidos x = True
    | otherwise = False
        where
            valoresPermitidos x = length (filter (>6) x) == 0 && length (filter (<1) x) == 0