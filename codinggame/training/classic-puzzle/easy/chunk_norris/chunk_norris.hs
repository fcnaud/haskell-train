import System.IO
import Data.List
import Data.Char
import Numeric

trans :: String -> String
trans str = prefix ++ " " ++ replicate len '0'
  where len    = length str
        prefix = if head str=='0' then "00" else "0"

growStr :: String -> String
growStr = \s -> if length s<7 then growStr ('0':s) else s

main :: IO()
main = do
  hSetBuffering stdout NoBuffering
  message <- getLine
  let dataList = concatMap (\x-> growStr $ showIntAtBase 2 intToDigit (ord x) "") message
  let ret      = unwords $ map trans $ group dataList

  putStrLn ret
