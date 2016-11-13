sumin :: Integer -> Integer
sumin n = sum $ map (\x -> x*((n-x)*2+1) ) [1..n]

sumax :: Integer -> Integer
sumax n = sum $ map (\x -> x*(x*2-1)) [1..n]

sumsum :: Integer -> Integer
sumsum n = sum $ map (*(2*n)) [1..n]
