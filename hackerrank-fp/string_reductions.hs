solve :: String -> String
solve [] = []
solve (x:xs) = x : solve [y|y<-xs, y/=x]

main = do
  str <- getLine
  putStrLn $ solve str
