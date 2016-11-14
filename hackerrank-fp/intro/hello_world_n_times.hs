main = do
  n <- readLn :: IO Int
  mapM_ (const $ putStrLn "Hello World") [1..n]
