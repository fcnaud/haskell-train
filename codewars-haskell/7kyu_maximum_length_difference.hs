getmami :: [String] -> (Int, Int)
getmami xs = (maximum ys, minimum ys)
  where ys = map length xs

mxdiflg :: [String] -> [String] -> Maybe Int
mxdiflg [] s2 = Nothing
mxdiflg s1 [] = Nothing
mxdiflg s1 s2 = Just $ max (abs $ fst p1 - snd p2) (abs $ fst p2 - snd p1)
  where p1 = getmami s1
        p2 = getmami s2
