import Data.List

longest :: String -> String -> String
longest xs ys = sort.nub $ xs ++ ys
