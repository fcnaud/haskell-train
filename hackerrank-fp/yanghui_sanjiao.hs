
getValue n m
  | m == 1 = 1
  | n == m = 1
  | otherwise = getValue (n-1) (m-1) + getValue (n-1) m

yangHui n = [getValue n x | x<- [1..n] ]

main = do
  n <- readLn :: IO Int
  mapM_ (putStrLn.unwords) [map show $ yangHui x| x<-[1..n]]
