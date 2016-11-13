seven :: Integer -> (Integer, Int)
seven n = (res, step)
  where xs   = iterate f n
        f    = \x -> x `div` 10 - (x`mod`10 * 2)
        res  = head $ dropWhile (>=100) xs 
        step = length $ takeWhile (>=100) xs
