module Parcial2 where

{-data BinTree a = TreeNode a (BinTree a) (BinTree a) | EmptyTree
    deriving (Show, Eq)
-}
foldTree f n (EmptyTree) = n
foldTree f n (TreeNode x l r) = ((foldTree f n r) `f` (foldTree f n l)) `f` x
{-
tree0 = EmptyTree
tree1 = (TreeNode 7 tree0 tree0)
tree2 = (TreeNode 3 tree1 tree0)-}

zipTree _ (EmptyTree) (EmptyTree) = (EmptyTree)
zipTree f tree (EmptyTree) = (EmptyTree)
zipTree f (EmptyTree) tree = (EmptyTree)
zipTree f (TreeNode x l1 r1) (TreeNode y l2 r2) = TreeNode (f x y) (zipTree f l1 l2) (zipTree f r1 r2)

filterMap f lista = map (\((Just x),_) -> x) (filter (\(x,y) -> x /= Nothing) (zip (map f lista) lista))

data Numerical = IntNum Int | DoubleNum Double

instance Show Numerical where
    show (IntNum n) = show n
    show (DoubleNum n) = show n

instance Eq Numerical where
    (IntNum n) == (DoubleNum m) = (convertirADouble n) == m
    (DoubleNum n) == (IntNum m) = n == (convertirADouble m)
    (DoubleNum n) == (DoubleNum m) = n == m
    (IntNum n) == (IntNum m) = n == m

{-instance Eq Numerical where
    (IntNum n) == (DoubleNum m) =  m == fromInteger (n/1.0)-}

data BinTree a = TreeNode a (BinTree a) (BinTree a) | EmptyTree
    deriving (Show, Eq)


fullOfNothing :: [Maybe a] -> Bool
fullOfNothing vs = all isNothing vs
  where isNothing Nothing = True
        isNothing _ = False


mapTree _ EmptyTree = EmptyTree
mapTree f (TreeNode v l r) = TreeNode (f v) (mapTree f l) (mapTree f r)
mapTree f (TreeNode v EmptyTree EmptyTree) = TreeNode v emptyMap emptyMap
    where emptyMap = mapTree f EmptyTree

pair a b = (a, b)
tree0 = EmptyTree
tree1 = (TreeNode 7 tree0 tree0)
tree2 = (TreeNode 3 tree1 tree0)



data DataA = A1 | A2 DataB deriving (Eq, Show)
data DataB = B1 | B2 DataA deriving (Eq, Show)

convertirADouble:: Int -> Double
convertirADouble x = fromIntegral x
