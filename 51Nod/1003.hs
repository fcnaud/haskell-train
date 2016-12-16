solve :: Int -> Int
solve n = sum $ takeWhile (>0) $ map (div n) xs
  where xs = iterate (*5) 5

main :: IO ()
main = do
  n <- readLn :: IO Int
  print $ solve n

