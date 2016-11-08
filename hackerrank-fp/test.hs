import Data.List

f :: Int -> Int -> Bool
f x y 
  | x == 1 = False
  | x == y = True
  | x `mod` y == 0 = False
  | otherwise = f x (y+1)

judgePrime :: String -> Bool
judgePrime str = let num = (read::String->Int) str in f num 2

judgeLeft :: String -> Bool
judgeLeft str 
  | str == [] = True
  | judgePrime str = judgeLeft $ init str
  | otherwise =  False

judge :: Int -> Bool
judge x =
  let num = show x
  in any (\x -> x) [judgeLeft num]

main = do
  {-print $ judgeLeft $ show 1111-}
  {-print $ judgePrime $ show 1111-}
  print $ f 100001 2


  
