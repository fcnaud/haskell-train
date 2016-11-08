import Control.Monad
judge :: String -> Int -> Int -> Int -> Int -> Bool
judge [] r g y b
  | r == g && y == b = True
  | otherwise = False

judge (x:xs) r g y b = case x of 'R' -> judge xs (r+1) g y b
                                 'G' -> judge xs r (g+1) y b
                                 'Y' -> judge xs r g (y+1) b
                                 'B' -> judge xs r g y (b+1)



main = do
  n <- readLn :: IO Int
  
  input  <- replicateM n getLine
  mapM_ print $ map (\xs -> judge xs 0 0 0 0) input
