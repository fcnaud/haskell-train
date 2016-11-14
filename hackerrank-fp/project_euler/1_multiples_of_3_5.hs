import Control.Monad

solve :: Int -> Int
solve n = getSum 3 three + (getSum 5 five) - getSum 15 fifteen
  where three   = n `div` 3
        five    = n `div` 5
        fifteen = n `div` 15
        getSum  = \x m -> (x + x*m) * m `div` 2

main = do
  n <- readLn :: IO Int
  dataList <- replicateM n (readLn::IO Int)
  mapM_ (print.solve.(+(-1))) dataList



