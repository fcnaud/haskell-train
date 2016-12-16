-- 这道题看来还非要按照题目中的顺序来，而不是满足条件即可

import Data.Function
import Data.List
import Data.Char

arrange :: String -> String
arrange xs = unwords $ concat $ zipWith (\a b -> a:[b]) xs1' xs2'
  where xs' = sortBy (on compare length) $ words xs
        (xs1, xs2) = splitAt (length xs' `div` 2) xs'
        xs1' = map (map toLower) xs1
        xs2' = map (map toUpper) xs2
