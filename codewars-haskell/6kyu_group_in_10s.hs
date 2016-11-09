import Data.List

groupIn10s :: [Int] -> [[Int]]
groupIn10s [] = []
groupIn10s xs = map tail $ groupBy f $ sort xs' 
  where xs'   = xs ++ (takeWhile (<= (maximum xs)) $ map (*10) [0..])
        f a b = (a`div`10)==(b`div`10)