import Data.List

longestConsec :: [String] -> Int -> String
longestConsec strarr k 
  | n==0 || k>n || k<=0 = ""
  | otherwise = concat $ take k $ drop len strarr
    where n   = length strarr
          xs  = map length strarr
          xs' = map getSum [0..n-k]
          getSum m = sum $ take k $ drop m xs
          len = length $ takeWhile (<maximum xs') xs'
