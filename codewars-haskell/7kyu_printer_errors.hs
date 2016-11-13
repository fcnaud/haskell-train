import Data.Char

printerError :: String -> String 
printerError s = n ++ "/" ++ d
  where n = show.length $ filter (>'m') s
        d = show.length $ s
