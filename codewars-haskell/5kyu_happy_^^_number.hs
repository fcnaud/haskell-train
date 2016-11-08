import Data.Set (Set,member,insert,fromList)
import Data.Char 

getNext :: Integer -> Integer
getNext n = toInteger.sum $ map (\x -> digitToInt x ^ 2) $ show n

checkHappy :: Integer -> Set Integer -> Bool
checkHappy n set = case member n set of
  True  -> False
  False -> if m==1 then True else let set' = insert n set in checkHappy m set'
  where m = getNext n

isHappy :: Integer -> Bool
isHappy n = checkHappy n (fromList [])
