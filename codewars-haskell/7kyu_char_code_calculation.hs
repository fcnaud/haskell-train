import Data.Char

calc :: String -> Int
calc str = num1 - num2
  where trans x = if x=='7' then '1' else x
        str' = concatMap (show.ord) str
        str''= map trans str'
        num1 = sum $ map digitToInt str'
        num2 = sum $ map digitToInt str''
