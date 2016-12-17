_mod :: Integer
_mod = 1000000007 

powMod :: Integer -> Integer -> Integer -> Integer
powMod a 0 m = 1
powMod a 1 m = a `mod` m
powMod a b m = halfPow * halfPow * haveOth `mod` m
  where halfPow = powMod a (b`div`2) m
        haveOth = if b`mod`2==0 then 1 else a`mod`m

main :: IO ()
main = do
  n <- readLn :: IO Integer
  print $ (powMod 3 (n+1) _mod - 1) * powMod 2 (_mod-2) _mod `mod` _mod
