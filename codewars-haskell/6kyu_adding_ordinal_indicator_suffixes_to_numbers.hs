getSuffix :: String -> String
getSuffix all@(_:x) = case num of
  0 -> ""
  11 -> "th"
  12 -> "th"
  13 -> "th"
  _  -> case num' of
    1 -> "st"
    2 -> "nd"
    3 -> "rd"
    _ -> "th"
  where num  = read all :: Int
        num' = read x  :: Int
        
        
numberToOrdinal :: Int -> String
numberToOrdinal n = xs ++ suf
  where xs  = show n
        len = length xs
        suf = getSuffix (if len==1 then '0':[last xs] else drop (len-2) xs)

