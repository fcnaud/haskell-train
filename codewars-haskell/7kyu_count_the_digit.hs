import Data.Char

getD:: Int -> Int -> Int
getD d n = length $ filter (==intToDigit d) $ show n

nbDig :: Int -> Int -> Int
nbDig n d = sum $ map (\x -> getD d x^2) [0..n]
