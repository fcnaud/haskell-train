getLast :: Int -> [Int] -> Int
getLast lsum [] = -1;
getLast lsum (x:xs)
  | lsum == sum xs = length xs
  | otherwise = getLast (lsum+x) xs

findEvenIndex :: [Int] -> Int
findEvenIndex xs
  | ans <= 0 = ans
  | otherwise = ans - 1
  where rightLen = getLast 0 xs
        ans      = if rightLen==(-1) then -1 else length xs - rightLen