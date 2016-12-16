main :: IO ()
main = do
  a <- readLn
  b <- readLn
  print $ a`div`b
  print $ a`mod`b
