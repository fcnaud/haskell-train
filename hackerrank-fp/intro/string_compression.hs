calcCh :: Char -> String -> Int
calcCh ch [] = 0
calcCh ch (x:xs)
  | x == ch = 1 + calcCh ch xs
  | otherwise = 0

toStr :: Int -> String
toStr 1 = []
toStr n = show n

solve :: String -> String
solve []         = []
solve all@(x:xs) = x : (toStr $ calcCh x all) ++ (solve $ dropWhile (\x' -> x'==x) xs)

main = do
  str <- getLine
  putStrLn $ solve str
