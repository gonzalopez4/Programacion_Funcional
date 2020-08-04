module SegundoParcial2017 where

-- ================================= PREGUNTA 1 =================================
{-
Se define el siguiente tipo data de Haskell para representar árboles binarios.
    data BinTree a = Node a (BinTree a) (BinTree a) | Leaf a | Empty
                     deriving (Eq, Show)
Definir una función que calcule la altura de un árbol dado. Es decir la máxima
distancia de la raíz a las hojas.
    height :: BinTree a -> Int
-}

data BinTree a = Node a (BinTree a) (BinTree a) | Leaf a | Empty
    deriving (Eq, Show)

height :: BinTree a -> Int
height (Empty) = 0
height (Leaf _) = 0
height (Node n Empty Empty) = 0
height (Node n l r) = (max (height l) (height r) ) + 1

tree = (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))
tree1 = Node 2 (Leaf 1) (Node 3 Empty (Node 4 Empty (Leaf 5)))

-- ==============================================================================


-- ================================= PREGUNTA 2 =================================
{-
Definir el tipo Viaje como un tipo data de Haskell, que permita representar si
una persona se movió de un punto a otro de la ciudad caminando, en auto o en
omnibus. De ser en omnibus, se debe incluir la línea que se tomó (e.g. 104, 316,
etc). Definir una función obtenga el número de línea de un valor de tipo Viaje,
retornando Nothing si éste no aplica.
    nroLinea :: Viaje -> Maybe String
-}

data Viaje = Caminando | Auto | Omnibus Int
    deriving (Show, Eq)

nroLinea :: Viaje -> Maybe String
nroLinea (Caminando) = Nothing
nroLinea (Auto) = Nothing
nroLinea (Omnibus linea) = Just (show linea)

-- ==============================================================================


-- ================================= PREGUNTA 3 =================================
{-
Se define el siguiente tipo data de Haskell para representar naipes de la baraja
española.
    data Naipe = Oros Int | Copas Int | Espadas Int | Bastos Int
                 deriving (Eq, Show)
El juego del Tute se basa en robar cartas del mazo para conseguir puntos. El
puntaje se calcula de la siguiente forma: cada as vale 11 puntos, cada tres vale
10, cada rey vale 4, cada caballo vale 3 y cada sota vale 2. El resto de las
cartas no valen. Definir la función puntajeTute, que calcula el puntaje de un
conjunto de naipes.
    puntajeTute :: [Naipe] -> Int
-}

data Naipe = Oros Int | Copas Int | Espadas Int | Bastos Int
    deriving (Eq, Show)


numNaipe:: Naipe -> Int
numNaipe (Oros n) = n
numNaipe (Copas n) = n
numNaipe (Espadas n) = n
numNaipe (Bastos n) = n

valorNaipe:: Naipe -> Int
valorNaipe naipe = valor (numNaipe naipe)
    where
        valor n
            | n == 1 = 11
            | n == 3 = 10
            | n == 12 = 4
            | n == 11 = 3
            | n == 10 = 2
            | otherwise = 0



puntajeTute :: [Naipe] -> Int
puntajeTute [] = 0
puntajeTute mazo = valorNaipe (head mazo) + puntajeTute (tail mazo)

cartas = [x y | x <- [Oros], y <- [1..12]]

-- ==============================================================================
