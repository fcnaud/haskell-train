import Control.Monad

{-
fibs :: [Int] -> Int -> [Int]
fibs all@(x:y:xs) maxn 
  | x+y > maxn = all
  | otherwise = f ((x+y):all) maxn
-}

fibs :: [Int]
fibs = 1 : 2 : (zipWith (+) fibs $ tail fibs)

solve :: Int -> Int
solve n = sum $ filter (even) $ takeWhile (<n) fibs

main = do
  n <- readLn :: IO Int
  dataList <- replicateM n (readLn::IO Int)
  mapM_ (print.solve) dataList
