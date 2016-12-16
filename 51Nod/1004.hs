powMod :: Int -> Int -> Int
powMod n 0 = 1
powMod n 1 = n `mod` 10
powMod n m = (halfPow * halfPow * haveOth) `mod` 10
  where halfPow = powMod n (m`div`2)
        haveOth = if m`mod`2==0 then 1 else (n`mod`10)

main :: IO ()
main = do
  num <- readLn :: IO Int
  print $ powMod (num`mod`10) num
