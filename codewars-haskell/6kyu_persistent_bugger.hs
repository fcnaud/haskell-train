import Data.Char

persistence :: Int -> Int
persistence n = 
  if n < 10 then 0
  else 1 + persistence n'
  where n' = foldl1 (*) $ map digitToInt $ show n