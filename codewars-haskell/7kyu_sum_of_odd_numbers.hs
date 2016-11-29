getSum :: Integer -> Integer -> Integer
getSum e n = (1+e)*n `div` 2

rowSumOddNumbers :: Integer -> Integer
rowSumOddNumbers n = getSum rnum rlen - getSum lnum llen
  where llen = getSum (n-1) (n-1)
        rlen = getSum n n
        lnum = llen * 2 - 1
        rnum = rlen * 2 - 1

