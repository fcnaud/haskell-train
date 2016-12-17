powMod :: Integer -> Integer -> Integer -> Integer
powMod a 0 m = 1
powMod a 1 m = a `mod` m
powMod a b m = halfPow * halfPow * haveOth `mod` m
  where halfPow = powMod a (b`div`2) m
        haveOth = if b`mod`2==0 then 1 else a`mod`m

main :: IO ()
main = do
  input <- getLine
  let [a,b,m] = map (read::String->Integer) $ words input
  print $ powMod a b m
