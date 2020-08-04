module RefUT2 where

codigoA :: Int -> [String]
codigoA y = take y (repeat (show y))

codigoB :: [Integer] -> [Integer]
codigoB [] = []
codigoB [x] = [x]
codigoB x = (last x): codigoB(init x)