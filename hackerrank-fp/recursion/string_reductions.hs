solve :: String -> String
solve [] = []
solve (x:xs) = x : solve [y|y<-xs, x/=y]

main = do
    str <- getLine
    putStrLn $ solve str