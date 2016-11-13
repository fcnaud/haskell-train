nbYear :: Int -> Double -> Int -> Int -> Int
nbYear p0 percent aug p = length $ takeWhile (<p) $ iterate f p0
  where f = \x -> aug + (truncate $ (fromIntegral x) * (1+percent/100))
