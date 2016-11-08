import Data.Char

accum :: String -> String
accum s = tail.concat $ map (getS) $ zip s [0..]
  where getS (ch,n) = let ch' = toLower ch 
                          ch''= toUpper ch' in 
                      '-' : ch'' :(take n $ repeat ch')
