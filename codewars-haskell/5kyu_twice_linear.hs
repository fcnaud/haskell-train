import Data.List

dbls :: [Integer]
dbls = 1 : getDbls 0 0 1

getDbls :: Int -> Int -> Integer -> [Integer]
getDbls ind2 ind3 pre = if mi/=pre then mi : nt else nt
  where x2 = 1 + 2 * (toInteger $ dbls!!ind2)
        x3 = 1 + 3 * (toInteger $ dbls!!ind3)
        mi = min x2 x3
        nt = if x2 < x3 then getDbls (ind2+1) ind3 mi else getDbls ind2 (ind3+1) mi

dblLinear :: Int -> Integer
dblLinear n = dbls!!n
