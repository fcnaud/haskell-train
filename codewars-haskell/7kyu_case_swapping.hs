import Data.Char

swap :: String -> String
swap = map trans
  where trans = \ch -> case isLetter ch of
                       False -> ch
                       True  -> case isUpper ch of
                                False -> toUpper ch
                                True  -> toLower ch
