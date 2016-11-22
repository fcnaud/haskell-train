scale :: String -> Int -> Int -> String
scale str k n = if length ret ==0 then ret else init ret   
  where deal = \s -> concatMap (replicate k) s
        ret  = unlines $ concatMap ((replicate n).deal) $ lines str

