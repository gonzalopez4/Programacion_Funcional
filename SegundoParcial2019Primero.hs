module SegundoParcial2019Primero where

allDistinct:: (Eq a) => [a] -> Bool
allDistinct [] = True
allDistinct (x:xs) = (length (filter (x==) xs) == 0 ) && allDistinct xs
--  allDistinct (x:xs) = (all (x/=) xs) && (allDistinct xs)

sumDistance:: [Double] -> [Double] -> Double
sumDistance [] [] = 0
sumDistance x y
    | length x == length y = abs((head x) - head(y)) + (sumDistance (tail x) (tail y))
    | otherwise = 1.0/0.0



data Tuple9 a = Tuple9 (a,a,a,a,a,a,a,a,a)
    deriving (Eq, Show)

type Sudoku = Tuple9 (Tuple9 Char)

sudokuRowFromString [c1,c2,c3,c4,c5,c6,c7,c8,c9] = Tuple9 (c1,c2,c3,c4,c5,c6,c7,c8,c9)
sudokuRowsFromString "" = []
sudokuRowsFromString str = (sudokuRowFromString first9) :(sudokuRowsFromString rest)
    where (first9, rest) = splitAt 9 str

sudokuFromString str = Tuple9 (r1,r2,r3,r4,r5,r6,r7,r8,r9)
    where [r1,r2,r3,r4,r5,r6,r7,r8,r9] = sudokuRowsFromString str

--test = sudokuFromString "1  2  3   a  b  c   z  y  x4  5  6   d  e  f   w  v  u7  8  9   g  h  i   t  s  r"
test = sudokuFromString "9      2  8 25 3  3  7    4 94   7 5   73  6    56 4    7  13  8 3      9    3  2"

quadrants :: Sudoku -> [String]
quadrants (Tuple9 (r1,r2,r3,r4,r5,r6,r7,r8,r9)) = [quadrant1,quadrant2,quadrant3,
                                                  quadrant4,quadrant5,quadrant6,
                                                  quadrant7,quadrant8,quadrant9]
    where
        quadrant1 = [getVal r1 1, getVal r1 2, getVal r1 3,
                     getVal r2 1, getVal r2 2, getVal r2 3,
                     getVal r3 1, getVal r3 2, getVal r3 3]
        quadrant2 = [getVal r1 4, getVal r1 5, getVal r1 6,
                     getVal r2 4, getVal r2 5, getVal r2 6,
                     getVal r3 4, getVal r3 5, getVal r3 6]
        quadrant3 = [getVal r1 7, getVal r1 8, getVal r1 9,
                     getVal r2 7, getVal r2 8, getVal r2 9,
                     getVal r3 7, getVal r3 8, getVal r3 9]
        quadrant4 = [getVal r4 1, getVal r4 2, getVal r4 3,
                     getVal r5 1, getVal r5 2, getVal r5 3,
                     getVal r6 1, getVal r6 2, getVal r6 3]
        quadrant5 = [getVal r4 4, getVal r4 5, getVal r4 6,
                     getVal r5 4, getVal r5 5, getVal r5 6,
                     getVal r6 4, getVal r6 5, getVal r6 6]
        quadrant6 = [getVal r4 7, getVal r4 8, getVal r4 9,
                     getVal r5 7, getVal r5 8, getVal r5 9,
                     getVal r6 7, getVal r6 8, getVal r6 9]
        quadrant7 = [getVal r7 1, getVal r7 2, getVal r7 3,
                     getVal r8 1, getVal r8 2, getVal r8 3,
                     getVal r9 1, getVal r9 2, getVal r9 3]
        quadrant8 = [getVal r7 4, getVal r7 5, getVal r7 6,
                     getVal r8 4, getVal r8 5, getVal r8 6,
                     getVal r9 4, getVal r8 5, getVal r9 6]
        quadrant9 = [getVal r7 7, getVal r7 8, getVal r7 9,
                     getVal r8 7, getVal r8 8, getVal r8 9,
                     getVal r9 7, getVal r9 8, getVal r9 9]

getVal :: Tuple9 a -> Int -> a
getVal (Tuple9 (r1,r2,r3,r4,r5,r6,r7,r8,r9)) 1 = r1
getVal (Tuple9 (r1,r2,r3,r4,r5,r6,r7,r8,r9)) 2 = r2
getVal (Tuple9 (r1,r2,r3,r4,r5,r6,r7,r8,r9)) 3 = r3
getVal (Tuple9 (r1,r2,r3,r4,r5,r6,r7,r8,r9)) 4 = r4
getVal (Tuple9 (r1,r2,r3,r4,r5,r6,r7,r8,r9)) 5 = r5
getVal (Tuple9 (r1,r2,r3,r4,r5,r6,r7,r8,r9)) 6 = r6
getVal (Tuple9 (r1,r2,r3,r4,r5,r6,r7,r8,r9)) 7 = r7
getVal (Tuple9 (r1,r2,r3,r4,r5,r6,r7,r8,r9)) 8 = r8
getVal (Tuple9 (r1,r2,r3,r4,r5,r6,r7,r8,r9)) 9 = r9


data BinTree a = Empty | Node a (BinTree a) (BinTree a)
    deriving(Eq, Show)

treeMap:: (a -> b) -> BinTree a -> BinTree b
treeMap f (Empty) = (Empty)
treeMap f (Node n l r) = (Node (f n) (treeMap f l) (treeMap f r))

isSorted [] = True
--isSorted (x:xs) = (all (>=x) xs) && (isSorted xs)
isSorted (x:xs) = (all (x<=) xs) && (isSorted xs)

f x y = zipWith (\a b -> a + b) x y

--f x xs = map (x : ) xs

p x = x > 0

-- foldr (\x y -> (p x) && y )  True [1,2,3]        es igual a        foldl (\x y -> x && (p y)) True [1,2,3]