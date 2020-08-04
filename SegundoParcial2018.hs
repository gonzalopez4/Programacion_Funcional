module SegundoParcial2018 where

-- ================================= PREGUNTA 6 =================================
{-
Se define el siguiente tipo data de Haskell para representar colores. Se tiene un
caso para los colores dados por sus tres componentes RGB (cada una de 0 a 255), y
luego otros casos para colores normalmente usados.
    data Color = RGB Int Int Int | Rojo | Verde | Azul | Blanco | Negro
Se desea una función average que promedie dos colores. Esto se consigue
promediando los 3 componentes RGB. Se puede suponer que existe una función
normalize, que dado un Color lo retorna siempre en RGB.
    average :: Color ‐> Color ‐> Color
    normalize :: Color ‐> Color
-}

data Color = RGB Int Int Int | Rojo | Verde | Azul | Blanco | Negro
    deriving (Eq, Show)



average :: Color -> Color -> Color
average (RGB a1 b1 c1) (RGB a2 b2 c2) = (RGB (div (a1+a2) 2) (div (b1+b2) 2) (div (c1+c2) 2) )
average x y = average (normalize x) (normalize y)



-- ==============================================================================

-- ================================ PREGUNTA 10 =================================
{-
Se define el siguiente tipo data de Haskell para representar colores. Se tiene un
caso para los colores dados por sus tres componentes RGB (cada una de 0 a 255), y
luego otros casos para colores normalmente usados.
    data Color = RGB Int Int Int | Rojo | Verde | Azul | Blanco | Negro
Definir una función que convierta cualquier color en el caso con sus tres
componentes RGB. Tener en cuenta que las componentes son (255,0,0) para el rojo,
(0,255,0) para el verde, (0,0,255) para el azul, (255,255,255) para el blanco y
(0,0,0) para el negro.
    normalize :: Color ‐> Color
-}

normalize :: Color -> Color
normalize (RGB a b c) = (RGB a b c)
normalize Rojo = (RGB 255 0 0)
normalize Verde = (RGB 0 255 0)
normalize Azul = (RGB 0 0 255)
normalize Blanco = (RGB 255 255 255)
normalize Negro = (RGB 0 0 0)

-- ==============================================================================

-- ================================ PREGUNTA 11 =================================
{-
Se define el siguiente tipo data de Haskell para representar un árbol de
directorios y archivos. El caso del archivo (File) incluye el nombre del archivo
y su tamaño. El caso de la carpeta (Folder) incluye el nombre de la carpeta y sus
componentes.
    data Elem = File String Int | Folder String [Elem]
Se desea una función size que calcule el tamaño total de un Elem.
    size :: Elem ‐> Int
-}

data Elem = File String Int | Folder String [Elem]

size :: Elem -> Int
size (File str n) = n
size (Folder str xs) = sum [size x | x <- xs]

test = (Folder "a" [File "a" 1, Folder "a" [File "a" 1, File "a" 1, File "a" 1, File "a" 1], File "a" 2, File "a" 3])
file1 = File "Vironcho.exe" 2
file2 = File "PhotshopCS3KeygenFullNoRip.rar" 100
file3 = File "asd.jpg" 50

folder0 = Folder "AltasPlenas" [file2,file3]
folder1 = Folder "Homework" [file1,folder0]
-- ==============================================================================

data AyB = A Int | B Int
    deriving (Eq)

fun :: Elem -> String -> Bool
fun (File n _) s = n == s
fun (Folder _ es) s = any (\e -> fun e s) es