import Data.List

sumConsecutives :: [Int] -> [Int]
sumConsecutives s = map sum $ group s