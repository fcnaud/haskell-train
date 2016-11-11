getPowerLast :: Integer -> Integer -> Integer
getPowerLast a b
  | b == 0 = 1
  | b == 1 = (a`mod`10)
  | otherwise = (halfPow * halfPow * haveOth)`mod`10
    where halfPow = getPowerLast a (b`div`2)
          haveOth = if (b`mod`2==0) then 1 else (a`mod`10)


lastDigit :: Integer -> Integer -> Integer
lastDigit a b = getPowerLast (a`mod`10) b