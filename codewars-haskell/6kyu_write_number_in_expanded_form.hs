import Data.Char
import Data.List

expandedForm :: Int -> String
expandedForm num = intercalate " + " $ map show ret
  where numList  = map digitToInt $ show num
        len      = length numList
        multi10  = reverse $ take len $ iterate (*10) 1
        ret      = filter (/=0) $ zipWith (*) numList multi10
