import Data.List

leaveDiff :: [String] -> [String]
leaveDiff [] = []
leaveDiff (x:xs) = x: (leaveDiff $ filter (\y -> x/=y) xs)

-- Sorry for the name of the function.
inArray :: [String] -> [String] -> [String]
inArray a1 a2 = sort $ filter (\str -> any (\str1 -> isInfixOf str str1) a2) $ leaveDiff a1