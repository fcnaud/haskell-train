f :: Int -> Int -> Bool
f lim n = n < lim

main = do
  contents <- getContents
  mapM_ print $ (\(n:arr) -> filter (f n) arr) $ map read $ words contents
