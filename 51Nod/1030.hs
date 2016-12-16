import Data.Char
trans :: Char -> Integer
trans x = toInteger $ if x>='0'&&x<='9' then digitToInt x
          else ord x - ord 'A' + 10


main :: IO ()
main = do
  num <- getLine
  -- foldr
  -- print $ fst $ foldr (\x (ret,n) -> (x*n+ret, n*36)) (0,1) $ map trans num
  print $ foldl1 (\ret x -> ret*36+x) $ map trans num
  
