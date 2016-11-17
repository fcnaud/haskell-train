import Data.Char

toJadenCase :: String -> String
toJadenCase js = unwords $ map (\(x:xs)->toUpper x : xs) $ words js 
        
