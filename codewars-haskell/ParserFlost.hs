import Data.Maybe

checkLegal :: String -> Bool
checkLegal [] = True
checkLegal (x:xs) = if x>'0' && x<'9' then checkLegal xs else False

delHeadZero :: String -> String
delHeadZero [] = []
delHeadZero all@(x:xs) = if x=='0' then (delHeadZero xs) else all

parserFloat :: String -> Maybe Float
parserFloat s = 
  let status = checkLegal s


main = do
  inputData <- getLine
  print $ checkLegal inputData
