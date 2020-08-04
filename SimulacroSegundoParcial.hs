module SimulacroSegundoParcial where

data IntBinTree = Node Int (IntBinTree) (IntBinTree) | Leaf Int | Empty
    deriving (Eq, Show)

inorder:: IntBinTree -> [Int]
inorder Empty = []
inorder (Leaf n) = [n]
inorder (Node n l r) = inorder (l) ++ [n] ++ inorder (r)

tree = (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))


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



data Punto = Punto2D Int Int | Punto3D Int Int Int deriving (Show)

data Either a b = Left a | Right b


fun (Left a) = a
fun (Right b) = b

{-
data Maybe a = Just a | Nothing
fun :: [Maybe a] -> [a]
fun ((Just a) : xs) = a : (fun xs)
fun (Nothing : xs) = fun xs
fun [] = []-}