xbonacci :: Num a => [a] -> Int -> [a]
xbonacci xs n = take n xbs
  where xbs = xs ++ (foldl1 (zipWith (+)) $ take (length xs) $ iterate tail xbs)
