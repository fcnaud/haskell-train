import Data.Char
import Data.List

getCount :: String -> [(Char,Int)]
getCount xs = zip ['a'..'z'] $ map (\s -> length s - 1) $ group.sort $ ['a'..'z'] ++ (filter isLower xs)

toShow :: (Char,Int,Int) -> String
toShow (ch,n,t)
  | t == 1 = "/1:"++xs
  | t == 2 = "/2:"++xs
  | t == 3 = "/=:"++xs
  where xs = take n $ repeat ch

mergeInfo :: (Char,Int) -> (Char,Int) -> (Char,Int,Int)
mergeInfo (ch1,n1) (ch2,n2)
  | max n1 n2 <= 1 = (ch1,0,0)
  | n1 == n2 = (ch1,n1,3)
  | n1 >  n2 = (ch1,n1,1)
  | n1 <  n2 = (ch2,n2,2)

cmp :: (Char,Int,Int) -> (Char,Int,Int) -> Ordering
cmp (ch1,n1,t1) (ch2,n2,t2)
  | n1 > n2 = LT
  | n1 < n2 = GT
  | t1 < t2 = LT
  | t1 > t2 = GT
  | ch1 < ch2 = LT
  | ch1 > ch2 = GT
  | otherwise = EQ

mix :: String -> String -> String
mix s1 s2 = if length ans == 0 then ans else tail ans
  where xs1 = getCount s1
        xs2 = getCount s2
        ans = concat $ map toShow $ sortBy cmp $ filter (\(_,n,_)->n/=0) $ zipWith mergeInfo xs1 xs2


