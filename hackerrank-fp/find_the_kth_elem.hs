getKthElem n xs
  | n > length xs = -1
  | otherwise = last $ take n 

main = do
  let xs = [1,2, 3,4, 5]
  n <- readLn :: [Int]
  putStrLn . show . last . take n xs
