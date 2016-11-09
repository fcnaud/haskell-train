padovans :: Int -> [Int] -> [Int]
padovans 0 xs = xs
padovans n all@(x:y:z:xs) = padovans (n-1) ((y+z):all)

padovan :: Int -> Int
padovan n
  | n <= 2 = 1
  | otherwise = head $ padovans (n-2) [1,1,1]