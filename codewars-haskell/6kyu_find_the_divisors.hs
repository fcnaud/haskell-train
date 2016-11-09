divisors :: (Show a, Integral a) => a -> Either String [a]
divisors a = if length xs == 0 then Left (show a ++ " is prime") else Right xs
  where xs =  filter (\x -> a`mod`x==0) [2..(a-1)]