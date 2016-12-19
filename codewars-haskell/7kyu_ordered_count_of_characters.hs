import Data.List
import Data.Char

orderedCount :: String -> [(Char, Int)]
orderedCount str = map (\ch -> (ch, countLetter ch)) $ nub str
  where countLetter = \ch -> length $ filter (==ch) str
