import Data.Char

digitalRoot :: Integer -> Integer
digitalRoot num =
  if num' <= (toInteger 9) then num' else digitalRoot num'
  where str  = show num
        num' = sum $ map (\ch -> (toInteger.ord) ch - (toInteger.ord) '0') str

main = do
  inputData <- readLn :: IO Integer
  print inputData
  print $ digitalRoot inputData
