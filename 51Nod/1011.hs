myGcd :: Int -> Int -> Int
myGcd a 0 = a
myGcd a b = myGcd b (a`mod`b)

main :: IO()
main = do
  input <- getLine
  let [a, b] = map (read::String->Int) $ words input
  print $ gcd a b
