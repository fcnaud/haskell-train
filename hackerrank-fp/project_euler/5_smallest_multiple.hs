import Control.Monad

myGcd :: Int -> Int -> Int
myGcd n m
  | n `mod` m == 0 = m
  | otherwise = myGcd m (n`mod`m)

myLcm :: Int -> Int -> Int
myLcm n m = (n * m) `div` (myGcd n m) 

solve :: Int -> Int
solve n = foldl1 myLcm [1..n]


main = do
  n <- readLn :: IO Int
  ansList <- replicateM n $ do
    num <- readLn :: IO Int
    return (solve num)
  
  mapM_ (putStrLn.show) ansList
