getLevel :: Int -> Int -> String
getLevel n m = speacLine ++ starLine ++ speacLine
  where speacLine = replicate (n-m) ' '
        starLine  = replicate (2*m-1) '*'
 
buildTower :: Int -> [String]
buildTower n = map (getLevel n) [1..n]