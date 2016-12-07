import Data.Char

sumFromString :: String -> Integer
sumFromString = sum.getIntFromString.map (\ch -> if ch=='E'||ch=='e' then 'a' else ch)

getIntFromString :: String -> [Integer]
getIntFromString str = if length str'==0 then [] else num : getIntFromString str''
  where str' = dropWhile (\ch -> not $ ch>='0'&&ch<='9') str
        [(num, str'')] = reads  str' :: [(Integer, String)]

