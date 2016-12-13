type Ratio a = (a, a)

convertFracs :: Integral a => [Ratio a] -> [Ratio a]
convertFracs xs = map trans xs
  where sndXs   = map snd xs
        lcmNum  = foldr1 lcm sndXs
        trans x = let n = lcmNum `div` (snd x) in (fst x * n, lcmNum)
