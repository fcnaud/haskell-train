f n arr = concat [ [x|_<-[1..n]] | x<-arr ]

main = do
  contents <- getContents
  mapM_ print $ (\(n:arr) -> f n arr) $ map read $ words contents
