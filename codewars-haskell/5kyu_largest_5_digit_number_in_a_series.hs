import Data.Char


subOne :: Char -> Char
subOne ch = chr (ord ch - 1)


getAllFive :: Char -> String -> [String]
getAllFive ch [] = []
getAllFive ch all@(x:xs)
  | len < 5 = []
  | otherwise = (take 5 all) : (getAllFive ch xs')
  where len = length all
        xs' = dropWhile (<ch) xs
        len' = length xs'
        
        
getDigit5 :: Char -> String -> [String]
getDigit5 ch xs
  | len < 5 = [xs]
  | len' == 5 = [xs']
  | len' <  5 = getDigit5 (subOne ch) xs
  | otherwise = getAllFive ch xs
  where xs' = dropWhile (<ch) xs
        len' = length xs'
        len = length xs
        
        
digit5 :: String -> Int
digit5 xs = maximum $ map (read::String->Int) $ getDigit5 '9' xs