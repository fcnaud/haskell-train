import Control.Monad

solve :: Int -> Int
solve n = a*a - b
  where a = sum [1..n]
        b = sum $ map (\x -> x*x) [1..n]

main = do
  n <- readLn :: IO Int
  ansList <- replicateM n $ do
    num <- readLn :: IO Int
    return (solve num)
    
  mapM_ (putStrLn.show) ansList

