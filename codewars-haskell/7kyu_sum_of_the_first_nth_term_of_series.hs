import Text.Printf

{-
leaveTow :: Double -> String
leaveTow num = show h ++ (tail $ take 4 $ show t)
  where num' = (realToFrac $ round $ num*100) / 100
        h    = truncate num'
        t    = num' - (realToFrac h)
-}

seriesSum :: Integer -> String
--seriesSum n = printf "%.2f" (sum $ map (1.0/) $ take (fromInteger n :: Int) [1.0,4.0..]) :: String
seriesSum n = (printf "%d" 1 :: String)

