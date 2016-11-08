-- 错误
guessSquareRoot :: Double -> Double -> Integer

guessSquareRoot des x = 
  if abs (x*x - des) <= 1.0 then 0
  else 1 + guessSquareRoot des x'
  where x' = (x + des/x) / 2
  
intRac :: Integer -> Integer -> Integer
intRac des x = guessSquareRoot (realToFrac des) (realToFrac x)
