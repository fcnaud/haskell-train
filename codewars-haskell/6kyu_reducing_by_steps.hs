gcdi :: Integer -> Integer -> Integer
gcdi = gcd
lcmu :: Integer -> Integer -> Integer
lcmu = lcm
som :: Integer -> Integer -> Integer
som = (+)
maxi :: Integer -> Integer -> Integer
maxi = max
mini :: Integer -> Integer -> Integer
mini = min

operArray :: (Integer -> Integer -> Integer) ->[Integer] -> Integer -> [Integer]
operArray fct arr i = tail $ scanl fct i arr 

