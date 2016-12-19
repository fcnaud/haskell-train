countPositivesSumNegatives :: Maybe [Int] -> [Int]
countPositivesSumNegatives Nothing   = []
countPositivesSumNegatives (just []) = []
countPositivesSumNegatives (Just xs) = [length $ filter (>0) xs, sum $ filter (<0) xs]
