
myDivCeiling :: Integer -> Integer -> Integer
myDivCeiling n m = if mo==0 then di else di + 1
  where di = n `div` m
        mo = n `mod` m
        
getList :: Integer -> Integer -> [String]
getList 0 y = []
getList 1 y = ["1/"++(show y)]
getList x y = 
  if x > y then
    (show (x`div`y)) : getList (x`mod`y) y
  else 
    ("1/"++(show y')) : getList x'' y''
  where y'  = myDivCeiling y x
        x'' = (-y)`mod`x
        y'' = y * y'

decompose :: String -> String -> [String]
decompose n d = getList (read n::Integer) (read d::Integer)
