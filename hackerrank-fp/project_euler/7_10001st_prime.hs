import Control.Monad

primes :: [Int]
primes = 2 : f [3,5..] where f (x:xs) = x : f[y|y<-xs, y`mod`x/=0]

main = do
  n <- readLn :: IO Int
  ansList <- replicateM n $ do
    input <- readLn :: IO Int
    return (last $ take input primes)

  mapM_ (putStrLn.show) ansList
