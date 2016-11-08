import Data.List

myCompare :: Char -> Char -> Ordering
myCompare x y
  | x == y = EQ
  | x < y  = GT
  | x > y  = LT

descendingOrder :: Integer -> Integer
descendingOrder num = (read::String->Integer) $ sortBy myCompare $ show num

main = do
  inputNum <- readLn :: IO Integer
  print $ descendingOrder inputNum
