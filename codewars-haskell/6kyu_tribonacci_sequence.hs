tribs :: Num a => Int -> [a] -> [a]
tribs 0 xs = xs
tribs n all@(x:y:z:xs) = tribs (n-1) ((x+y+z):all)

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) n
  | n == 0 = []
  | n == 1 = [a]
  | n == 2 = [a,b]
  | n == 3 = [a,b,c]
  | otherwise = reverse $ tribs (n-3) [c,b,a]