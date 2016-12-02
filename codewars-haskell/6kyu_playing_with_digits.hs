import Data.Char

digpow :: Integer -> Integer -> Integer
digpow x p = if x'`mod`x==0 then x'`div`x else -1
  where x' = sum $ map (\(a,b)->b^a) $ zip [p..] $ map (fromIntegral.digitToInt) $ show x
