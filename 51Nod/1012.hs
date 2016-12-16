myLcm :: Int -> Int -> Int
myLcm a b = a `div` (myGcd a b) * b

myGcd :: Int -> Int -> Int
myGcd a 0 = a
myGcd a b = myGcd b (a`mod`b)

main :: IO ()
main = do
  input <- getLine
  let [a, b] = map (read::String->Int) $ words input
  print $ lcm a b
