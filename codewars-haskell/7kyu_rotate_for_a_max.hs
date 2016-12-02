
rot :: String -> String
rot [] = []
rot (x:xs) = xs++[x]

maxRot :: Integer -> Integer
maxRot n = maximum $ map (read::String->Integer) list
  where xs = show n
        list = foldl deal [xs] [0..length xs-1]
        deal = \ys m -> let xs' = head ys in (take m xs' ++ (rot $ drop m xs')) : ys 

