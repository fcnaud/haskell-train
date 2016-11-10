-- 这道题他给的用例是反的，所以就反着写了

import Prelude hiding (dropWhile)

dropWhile :: [a] -> (a->Bool) -> [a]
dropWhile [] p = []
dropWhile all@(x:xs) p = case (p x) of
  True  -> dropWhile xs p
  False -> all
