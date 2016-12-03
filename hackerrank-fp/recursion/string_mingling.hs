main = do
    line1 <- getLine
    line2 <- getLine
    mapM_ putStr $ zipWith (\x y -> [x,y]) line1 line2