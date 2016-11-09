import Data.Char

quickPow :: Int -> Int -> Integer
quickPow n 0 = 1
quickPow n 1 = toInteger n
quickPow n m = halfPow * halfPow * (toInteger haveOther)
  where halfPow = quickPow n (m`div`2)
        haveOther = if (m`mod`2==0) then 1 else n

narcissistic :: Integral n => n -> Bool
narcissistic n = toInteger n == (sum $ map (\x -> quickPow (digitToInt x) digit) xs)
  where xs     = show $ toInteger n
        digit  = length xs