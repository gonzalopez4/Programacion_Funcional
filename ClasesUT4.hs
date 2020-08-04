module ClasesUT4 where

import Data.List

data Month = Enero | Febrero | Marzo | Abril | Mayo | Junio | Julio | Agosto | Setiembre | Octubre | Noviembre | Diciembre
    deriving (Eq, Show)

data HoroscopeSign = Aries | Tauro | Géminis | Cáncer | Leo |  Virgo | Libra | Escorpio | Sagitario | Capricornio | Acuario | Piscis
    deriving (Eq, Show)


startDate, endDate:: HoroscopeSign -> (Int, Month)
startDate Capricornio = (20, Enero)
startDate Acuario = (21, Febrero)
startDate Piscis = (20, Marzo)
startDate Aries = (21, Abril)
startDate Tauro = (21, Mayo)
startDate Géminis =(22, Junio)
startDate Cáncer = (22, Julio)
startDate Leo = (24, Agosto)
startDate Virgo = (24, Setiembre)
startDate Libra = (24, Octubre)
startDate Escorpio = (24, Noviembre)
startDate Sagitario = (24, Diciembre)

endDate Aries = (20, Abril)
endDate Tauro = (20, Mayo)
endDate Géminis = (21, Junio)
endDate Cáncer = (21, Julio)
endDate Leo = (21, Agosto)
endDate Virgo = (22, Setiembre)
endDate Libra = (22, Octubre)
endDate Escorpio = (22, Noviembre)
endDate Sagitario = (21, Diciembre)
endDate Capricornio = (19, Enero)
endDate Acuario = (18, Febrero)
endDate Piscis = (20, Marzo)

data UKWeight = Stones Double | Pounds Double | Ounces Double

toOunces, toPounds, toStones:: UKWeight -> UKWeight
toOunces (Pounds x) = Ounces (x*16)
toOunces (Ounces x) = Ounces x
toOunces (Stones x) = Ounces (x*224)
toStones (Pounds x) = Stones (x*0.07)
toStones (Ounces x) = Stones (x*0.0044)
toStones (Stones x) = Stones x
toPounds (Pounds x) = Pounds x
toPounds (Ounces x) = Pounds (x*0.0625)
toPounds (Stones x) = Pounds (x*14)

data BinTree a = Node a (BinTree a) (BinTree a) | Leaf a | Empty
    deriving (Eq, Show)

preorder, inorder, postorder:: BinTree a -> [a]
preorder Empty = []
preorder (Leaf x) = [x]
preorder (Node x l r) = x : (preorder l ++ preorder r)

inorder Empty = []
inorder (Leaf x) = [x]
inorder (Node x l r) = inorder l ++ [x] ++  inorder r

postorder Empty = []
postorder (Leaf x) = [x]
postorder (Node x l r) = postorder l ++ postorder r ++ [x]

binTreeSearch:: Ord a => BinTree a -> a -> Bool
binTreeSearch Empty _ = False
binTreeSearch (Leaf x) n = x == n
binTreeSearch (Node x l r) n
    | x < n = binTreeSearch l n
    | x > n = binTreeSearch r n
    | otherwise = True



data Naipe =  Oro Int | Copa Int | Espada Int | Basto Int
    deriving (Eq, Show)


numberNaipe, suitNaipe:: Naipe -> Int
numberNaipe (Oro n) = n
numberNaipe (Copa n) = n
numberNaipe (Espada n) = n
numberNaipe (Basto n) = n
suitNaipe (Oro n) = 1
suitNaipe (Copa n) = 2
suitNaipe (Espada n) = 3
suitNaipe (Basto n) = 4

baraja = [x y | x <- [Oro, Copa, Espada, Basto], y <- [1..12]]

testMesa = [x y | x <- [Oro], y <- [5..8]]
testMano = Oro 12


turnoEscoba15:: [Naipe] -> Naipe -> [Naipe]
turnoEscoba15 mesa n
    | sum (map numberNaipe (n:mesa)) == 15 = []
    | sum (map numberNaipe (n:mesa)) < 15 = n : mesa
    | otherwise = if (resultado == []) then (n : mesa) else (mesa \\ head resultado)
        where
            resultado = [x | x <- (subsequences mesa), ((sumaNaipes x) + (numberNaipe n)) == 15]
            sumaNaipes naipes
                | naipes == [] = 0
                | otherwise = numberNaipe (head naipes) + sumaNaipes (tail naipes)

instance Show UKWeight where
    show (Stones n) = (show n) ++ "st"
    show (Pounds n) = (show n) ++ "lbs"
    show (Ounces n) = (show n) ++ "oz"

instance Eq UKWeight where
    (Stones n) == (Stones m) = abs(n-m) < 1e-15
    (Pounds n) == (Pounds m) = abs(n-m) < 1e-15
    (Ounces n) == (Ounces m) = abs(n-m) < 1e-15
    n == m = (toOunces n) == (toOunces m)

getValor:: UKWeight -> Double
getValor (Stones n) = n
getValor (Pounds n) = n
getValor (Ounces n) = n

instance Num UKWeight where
    abs (Stones n)
        | n >= 0 = (Stones n)
        | otherwise = (Stones ((-1) * (n)))
    abs (Pounds n)
        | n >= 0 = (Pounds n)
        | otherwise = (Pounds ((-1) * (n)))
    abs (Ounces n)
        | n >= 0 = (Ounces n)
        | otherwise = (Ounces ((-1) * (n)))

    signum (Stones n)
        | n > 0 = 1
        | n == 0 = 0
        | otherwise = (-1)
    signum (Pounds n)
        | n > 0 = 1
        | n == 0 = 0
        | otherwise = (-1)
    signum (Ounces n)
        | n > 0 = 1
        | n == 0 = 0
        | otherwise = (-1)

    (Stones n) - (Stones m) = (Stones (n-m))
    (Pounds n) - (Pounds m) = (Pounds (n-m))
    (Ounces n) - (Ounces m) = (Ounces (n-m))
    (Stones n) - m = (Stones n) - (toStones m)
    (Ounces n) - m = (Ounces n) - (toOunces m)
    (Pounds n) - m = (Pounds n) - (toPounds m)

    (Stones n) + (Stones m) = (Stones (n+m))
    (Pounds n) + (Pounds m) = (Pounds (n+m))
    (Ounces n) + (Ounces m) = (Ounces (n+m))
    (Stones n) + m = (Stones n) + (toStones m)
    (Ounces n) + m = (Ounces n) + (toOunces m)
    (Pounds n) + m = (Pounds n) + (toPounds m)

    (Stones n) * (Stones m) = (Stones (n*m))
    (Pounds n) * (Pounds m) = (Pounds (n*m))
    (Ounces n) * (Ounces m) = (Ounces (n*m))
    (Stones n) * m = (Stones n) * (toStones m)
    (Ounces n) * m = (Ounces n) * (toOunces m)
    (Pounds n) * m = (Pounds n) * (toPounds m)

    fromInteger n = (Pounds (fromInteger (n)))

beats:: Naipe -> Naipe -> Bool
beats (Espada 1) _ = True

beats (Basto 1) (Espada 1) = False
beats (Basto 1) _ = True

beats (Espada 7) (Espada 1) = False
beats (Espada 7) (Basto 1) = False
beats (Espada 7) (Oro 7) = False
beats (Espada 7) _ = True

beats (Oro 7) (Espada 1) = False
beats (Oro 7) (Basto 1) = False
beats (Oro 7) (Espada 7) = False
beats (Oro 7) _ = True

beats _ (Espada 1) = False
beats _ (Basto 1) = False
beats _ (Oro 7) = False
beats _ (Espada 7) = False

beats n m
    | numberNaipe n > numberNaipe m = True
    | otherwise = False

