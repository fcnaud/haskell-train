
solve :: Int -> Int -> Int
solve n p = foldr1 (\ret x -> (ret*x)`mod`p) [1..n]

main :: IO ()
main = do
  input <- getLine
  let [n,p] = map (read::String->Int) $ words input

  print $ solve n p
