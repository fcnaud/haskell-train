import Control.Monad

f :: Int -> Int -> Int
f n m 
  | n <= m = n
  | n `mod` m == 0 = f (n `div` m) m
  | otherwise = f n (m+1)

main = do
  n <- readLn :: IO Int
  dataList <- replicateM n (readLn::IO Int)
  mapM_ (print.(\x -> f x 2)) dataList
