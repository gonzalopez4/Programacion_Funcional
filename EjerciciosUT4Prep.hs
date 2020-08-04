module EjerciciosUT4Prep where

import Data.List

data Month = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Eq, Show, Enum)

{- instance Enum Month where
    succ January = February
    succ February = March
    succ March = April
    succ April = May
    succ May = June
    succ June = July
    succ July = August
    succ August = September
    succ September = October
    succ October = November
    succ November = December
    succ December = January

    pred January = December
    pred February = January
    pred March = February
    pred April = March
    pred May = April
    pred June = May
    pred July = June
    pred August = July
    pred September = August
    pred October = September
    pred November = October
    pred December = November
-}

nextMonth :: Month -> Int -> Month
nextMonth month n
    | n > 0 = nextMonth (succ month) (n-1)
    | n == 0 = month
    | otherwise = nextMonth (pred month) (n+1)

previousMonth :: Month -> Int -> Month
previousMonth month n
    | n > 0 = previousMonth (pred month) (n-1)
    | n == 0 = month
    | otherwise = previousMonth (succ month) (n+1)


data Colour = Rojo | Azul | Verde | Blanco | Negro | RGB Int Int Int
    deriving (Show)

toRGB :: Colour -> Colour
toRGB Rojo = RGB 255 0 0
toRGB Azul = RGB 0 0 225
toRGB Verde = RGB 0 225 0
toRGB Blanco = RGB 225 225 225
toRGB Negro = RGB 0 0 0
toRGB (RGB a b c) = (RGB a b c)

red:: Colour -> Int
red (RGB a b c) = a
red n = red (toRGB n)

green :: Colour -> Int
green (RGB a b c) = b
green n = green (toRGB n)

blue :: Colour -> Int
blue (RGB a b c) = c
blue n = blue (toRGB n)


data AstroDistance = AstroUnits Double | LightYears Double | Parsecs Double
    deriving (Show)

toAstroUnits:: AstroDistance -> AstroDistance
toAstroUnits (LightYears n) = (AstroUnits (n * 63241))
toAstroUnits (AstroUnits n) = (AstroUnits n)
toAstroUnits (Parsecs n) = (AstroUnits (n * 210000))


toLightYears:: AstroDistance -> AstroDistance
toLightYears (AstroUnits n) = (LightYears (n / 63241))
toLightYears (LightYears n) = (LightYears n)
toLightYears (Parsecs n) = (LightYears (n * 3.26156))

toParsecs:: AstroDistance -> AstroDistance
toParsecs (AstroUnits n) = (Parsecs (n * 4.84815e-6))
toParsecs (Parsecs n) = (Parsecs n)
toParsecs (LightYears n) = (Parsecs (n * 0.306601))

asAstroUnits:: AstroDistance -> Double
asAstroUnits (AstroUnits n) = n
asAstroUnits n = asAstroUnits(toAstroUnits n)

asLightYears:: AstroDistance -> Double
asLightYears (LightYears n) = n
asLightYears n = asLightYears(toLightYears n)

asParsecs:: AstroDistance -> Double
asParsecs (Parsecs n) = n
asParsecs n = asParsecs(toParsecs n)

data Naipe = Oros Int | Copas Int | Espadas Int | Bastos Int
    deriving (Show, Eq)

numNaipe, suitNaipe:: Naipe -> Int
numNaipe (Oros n) = n
numNaipe (Copas n) = n
numNaipe (Espadas n) = n
numNaipe (Bastos n) = n
suitNaipe (Oros n) = 1
suitNaipe (Copas n) = 2
suitNaipe (Espadas n) = 3
suitNaipe (Bastos n) = 4

baraja = [x y | x <- [Oros, Copas, Espadas, Bastos], y <- [1..12]]


data BinTree a = Empty | Node a (BinTree a) (BinTree a)
    deriving (Show, Eq)

tree = (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))

preorder, inorder, postorder:: BinTree a -> [a]
preorder Empty = []
preorder (Node x Empty Empty) = [x]
preorder (Node x l r) = x : (preorder l ++ preorder r)

inorder Empty = []
inorder (Node x Empty Empty) = [x]
inorder (Node x l r) = inorder l ++ [x] ++  inorder r

postorder Empty = []
postorder (Node x Empty Empty) = [x]
postorder (Node x l r) = postorder l ++ postorder r ++ [x]

searchInBinTree:: Ord a => BinTree a -> a -> Bool
searchInBinTree Empty _ = False
searchInBinTree (Node x Empty Empty) n = x == n
searchInBinTree (Node x l r) n
    | x < n = searchInBinTree l n
    | x > n = searchInBinTree r n
    | otherwise = True

isSortedBinTree:: Ord a => BinTree a -> Bool
isSortedBinTree arbol = isSorted (inorder arbol)
    where
        isSorted [] = True
        isSorted [x] = True
        isSorted (x:y:xs) = x <= y && isSorted (y:xs)

insertOrdBinTree:: Ord a => BinTree a -> a -> BinTree a
insertOrdBinTree Empty a = Node a Empty Empty
insertOrdBinTree (Node x l r) a
    | x > a = Node x (insertOrdBinTree l a) r
    | x < a = Node x l (insertOrdBinTree r a)
    | otherwise = error "Ya existe este valor en el arbol"


instance Eq AstroDistance where
    (AstroUnits x) == (AstroUnits y) = x == y
    (LightYears x) == (LightYears y) = x == y
    (Parsecs x) == (Parsecs y) = x == y
    x == y = (toAstroUnits x) == (toAstroUnits y)

instance Ord AstroDistance where

    x < y = (toAstroUnits x) < (toAstroUnits y)
    x > y = (toAstroUnits x) > (toAstroUnits y)
    x <= y = (toAstroUnits x) <= (toAstroUnits y)
    x >= y = (toAstroUnits x) >= (toAstroUnits y)

instance Num AstroDistance where

    (AstroUnits x) + (AstroUnits y) = AstroUnits (x + y)
    (Parsecs x) + (Parsecs y) = Parsecs (x + y)
    (LightYears x) + (LightYears y) = LightYears (x + y)

    (AstroUnits x) + y = (AstroUnits x) + (toAstroUnits y)
    (Parsecs x) + y = (Parsecs x) + (toParsecs y)
    (LightYears x) + y = (LightYears x) + (toLightYears y)


    (AstroUnits x) - (AstroUnits y) = AstroUnits (x - y)
    (Parsecs x) - (Parsecs y) = Parsecs (x - y)
    (LightYears x) - (LightYears y) = LightYears (x - y)

    (AstroUnits x) - y = (AstroUnits x) - (toAstroUnits y)
    (Parsecs x) - y = (Parsecs x) - (toParsecs y)
    (LightYears x) - y = (LightYears x) - (toLightYears y)


    (AstroUnits x) * (AstroUnits y) = AstroUnits (x * y)
    (Parsecs x) * (Parsecs y) = Parsecs (x * y)
    (LightYears x) * (LightYears y) = LightYears (x * y)

    (AstroUnits x) * y = (AstroUnits x) * (toAstroUnits y)
    (Parsecs x) * y = (Parsecs x) * (toParsecs y)
    (LightYears x) * y = (LightYears x) * (toLightYears y)


    abs (AstroUnits x)
        | x >= 0 = (AstroUnits x)
        | otherwise = (AstroUnits ((-1)*x))

    abs (Parsecs x)
        | x >= 0 = (Parsecs x)
        | otherwise = (Parsecs ((-1)*x))

    abs (LightYears x)
        | x >= 0 = (LightYears x)
        | otherwise = (LightYears ((-1)*x))

    signum (AstroUnits x)
        | x > 0 = 1
        | x == 0 = 0
        | otherwise = (-1)

    signum (Parsecs x)
        | x > 0 = 1
        | x == 0 = 0
        | otherwise = (-1)

    signum (LightYears x)
        | x > 0 = 1
        | x == 0 = 0
        | otherwise = (-1)

    fromInteger n = (LightYears (fromInteger (n)))

instance Fractional AstroDistance where

    (AstroUnits x) / (AstroUnits y) = AstroUnits (x / y)
    (Parsecs x) / (Parsecs y) = Parsecs (x / y)
    (LightYears x) / (LightYears y) = LightYears (x / y)

    (AstroUnits x) / y = (AstroUnits x) / (toAstroUnits y)
    (Parsecs x) / y = (Parsecs x) / (toParsecs y)
    (LightYears x) / y = (LightYears x) / (toLightYears y)

    recip (AstroUnits n) = AstroUnits(1/n)
    recip (Parsecs n) = Parsecs(1/n)
    recip (LightYears n) = LightYears(1/n)

    fromRational n = (LightYears (fromRational (n)))
