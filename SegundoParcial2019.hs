module SegundoParcial2019 where
-- ================================= PREGUNTA 1 =================================
{-
Definir un tipo data de Haskell ( llamado IntBinTree) que represente árboles
binarios con números enteros en sus hojas y nodos internos. Luego, definir la
función inorder que recorre todo un árbol binario y retorna sus números en
inorden.
    inorder :: IntBinTree -> [Int]
-}

data IntBinTree = Node Int (IntBinTree) (IntBinTree) | Leaf Int | Empty
    deriving (Eq, Show)

inorder:: IntBinTree -> [Int]
inorder Empty = []
inorder (Leaf n) = [n]
inorder (Node n l r) = inorder (l) ++ [n] ++ inorder (r)

tree = (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))


-- ==============================================================================


-- ================================= PREGUNTA 2 =================================
{-
Definir un tipo data de Haskell (llamado Temperature) que represente temperaturas
que se pueden expresar en las escalas Farenheit, Celsius y Kelvin. Cada caso debe
tener un argumento de tipo Double. Luego, definir las funciones de conversión
entre las escalas, de las siguiente forma.
    inCelsius :: Temperature -> Temperature -- Convierte a Celsius.
    inFarenheit :: Temperature -> Temperature -- Convierte a Farenheit.
    inKelvin :: Temperature -> Temperature -- Convierte a Kelvin.
Tener en cuenta que el 0º Celsius son 32º Farenheit y 273.15º Kelvin, mientras
que 100º Celsius son 212º Farenheit y 373.15º Kelvin.
-}

data Temperature = Farenheit Double | Celsius Double | Kelvin Double
    deriving (Eq, Show)


inCelsius, inFarenheit, inKelvin :: Temperature -> Temperature
inCelsius (Celsius x) = (Celsius x)
inCelsius (Farenheit x) = (Celsius ((x - 32) * (5/9)))
inCelsius (Kelvin x) = (Celsius (x - 273.15))

inFarenheit (Farenheit x) = (Farenheit x)
inFarenheit (Celsius x) = (Farenheit (x* (9 / 5) + 32))
inFarenheit (Kelvin x) = (Farenheit ((x - 273.15) * (9 / 5) + 32))

inKelvin (Kelvin x) = (Kelvin x)
inKelvin (Farenheit x) = (Kelvin ( ( (x-32) * (5/9) ) + 273.15 ) )
inKelvin (Celsius x) = (Kelvin (x + 273.15))


-- ==============================================================================


-- ================================= PREGUNTA 3 =================================
{-
Se define un tipo de dato de Haskell para representar tableros del juego Ajedrez,
de la siguiente forma. Cada tupla en la lista del tablero indica el rol, tipo de
la pieza, así como las fila y columna donde está ubicada.
    data Rol = Blancas | Negras
    data Casilla = Vacia | Peon Rol | Caballo Rol | Alfil Rol
    | Torre Rol | Reina Rol | Rey Rol
    data Tablero = Tablero [Casilla]
Definir al tipo Tablero como una instancia de la clase Show. El tablero se debe
imprimir con un punto por cada casilla vacía, una letra P para cada peón, C para
cada caballo, A para cada alfil, T para cada torre, D para cada dama y R para
cada rey. Las blancas llevan letras mayúsculas y las negras letras minúsculas.
-}

data Rol = Blancas | Negras
data Casilla = Vacia | Peon Rol | Caballo Rol | Alfil Rol | Torre Rol | Reina Rol | Rey Rol
data Tablero = Tablero [Casilla]

instance Show Casilla where
    show(Vacia) = "."
    show(Peon Blancas) = "P"
    show(Peon Negras) = "p"
    show(Caballo Blancas) = "C"
    show(Caballo Negras) = "c"
    show(Alfil Blancas) = "A"
    show(Alfil Negras) = "a"
    show(Torre Blancas) = "T"
    show(Torre Negras) = "t"
    show(Reina Blancas) = "D"
    show(Reina Negras) = "d"
    show(Rey Blancas) = "R"
    show(Rey Negras) = "r"

instance Show Tablero where
    show(Tablero []) = []
    show(Tablero xs)= imprimirFila(take 8 xs) ++ "\n" ++ show (Tablero (drop 8 xs))
        where
            imprimirFila [] = []
            imprimirFila x = (show (head x)) ++ imprimirFila (tail x)

tablero = Tablero [Torre Blancas,Caballo Blancas,Alfil Blancas,Rey Blancas,Reina Blancas,Alfil Blancas,Caballo Blancas,Torre Blancas,
    Peon Blancas,Peon Blancas,Peon Blancas,Peon Blancas,Peon Blancas,Peon Blancas,Peon Blancas,Peon Blancas,
    Vacia,Vacia,Vacia,Vacia,Vacia,Vacia,Vacia,Vacia,
    Vacia,Vacia,Vacia,Vacia,Vacia,Vacia,Vacia,Vacia,
    Vacia,Vacia,Vacia,Vacia,Vacia,Vacia,Vacia,Vacia,
    Vacia,Vacia,Vacia,Vacia,Vacia,Vacia,Vacia,Vacia,
    Peon Negras,Peon Negras,Peon Negras,Peon Negras,Peon Negras,Peon Negras,Peon Negras,Peon Negras,
    Torre Negras,Caballo Negras,Alfil Negras,Rey Negras,Reina Negras,Alfil Negras,Caballo Negras,Torre Negras]

-- Para imprimir lindo, usar putStr $ show tablero
-- ==============================================================================