main :: IO()
main = do
  input <- getLine
  print $ sum $ map (read::String->Int) $ words input
