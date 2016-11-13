nbYear :: Int -> Double -> Int -> Int -> Int
nbYear p0 percent aug p
  | p0 >= p = 0
  | otherwise = 1 + nbYear (np) percent aug p
  where np = p0 + (truncate $ (realToFrac p0) * (percent/100)) + aug
