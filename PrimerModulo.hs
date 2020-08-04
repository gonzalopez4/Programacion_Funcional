module PrimerModulo where

holaMundo = putStrLn "Hola mundo"

suma x y = x + y

resta x y = x - y

rule3 0 y z = error("Esta Mal")

rule3 x y z = (y * z)/x

clamp left right n = if left > right then error ("rango alreves") else if n < left then left else if n > right then right else n

diceChar :: Int -> Char

diceChar x
    | x > 0 && x < 7 =  toEnum(0x2679 + x)
    | otherwise = error "mal"