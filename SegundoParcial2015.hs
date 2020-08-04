module SegundoParcial2015 where


data Color = RGB Int Int Int | Rojo | Verde | Azul | Blanco | Negro
    deriving (Eq, Show)



average :: Color -> Color -> Color
average (RGB a1 b1 c1) (RGB a2 b2 c2) = (RGB (div (a1+a2) 2) (div (b1+b2) 2) (div (c1+c2) 2) )
average x y = average (normalize x) (normalize y)

normalize :: Color -> Color
normalize (RGB a b c) = (RGB a b c)
normalize Rojo = (RGB 255 0 0)
normalize Verde = (RGB 0 255 0)
normalize Azul = (RGB 0 0 255)
normalize Blanco = (RGB 255 255 255)
normalize Negro = (RGB 0 0 0)


data Elem = File String Int | Folder String [Elem]

size:: Elem -> Int
size (File _ n) = n
size (Folder _ xs) = sum [size x | x <- xs]

test = (Folder "a" [File "a" 1, Folder "a" [File "a" 1, File "a" 1, File "a" 1, File "a" 1], File "a" 2, File "a" 3])
file1 = File "Vironcho.exe" 2
file2 = File "PhotshopCS3KeygenFullNoRip.rar" 100
file3 = File "asd.jpg" 50

folder0 = Folder "AltasPlenas" [file2,file3]
folder1 = Folder "Homework" [file1,folder0]

fun:: Elem -> String -> Bool
fun (File n _) s = n == s
fun (Folder _ es) s = any (\e -> fun e s) es