maxSequence :: [Int] -> Int
maxSequence xs = getMaxSum 0 0 xs

getMaxSum :: Int -> Int -> [Int] -> Int
getMaxSum bestAns curMax [] = max bestAns curMax
getMaxSum bestAns curMax (x:xs)
  | curMax+x < 0 = getMaxSum (max bestAns curMax) 0 xs
  | otherwise    = getMaxSum (max bestAns curMax) (curMax+x) xs