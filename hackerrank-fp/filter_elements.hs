solve :: [Int] -> Int -> [Int]
solve [] _ = []
solve all@(x:xs) k = 
  case (length $ filter (\y -> y==x) all) >= k 
    of True -> x : solve (filter (\y -> y/=x) xs) k
       False -> solve (filter (\y -> y/=x) xs) k

deal :: IO ()
deal = do
  input <- getLine
  let k =  (read::String->Int) $ (words input)!!1
  input <- getLine
  let ans = solve (map (read::String->Int) $ words input) k
  case length ans of 0 -> print -1
                     _ -> putStrLn.unwords $ map show ans

main = do
  n <- readLn :: IO Int
  mapM_ (const deal) [1..n]
