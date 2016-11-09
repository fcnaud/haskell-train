import Data.Char

iswrap :: Char -> Int -> Bool
iswrap ch shift
  | isLower ch = ord ch + shift > ord 'z'
  | isUpper ch = ord ch + shift > ord 'Z'
  
trans :: Char -> Int -> Char
trans ch shift
  | isLetter ch = ch'
  | otherwise = ch
  where shift' = shift `mod` 26
        ch'    = if iswrap ch shift' then chr (ord ch - 26 + shift')
                 else chr (ord ch + shift')

cipher :: String -> Int -> String
cipher str k = map (\ch -> trans ch k) str

main :: IO()
main = do
  n <- readLn :: IO Int
  s <- getLine
  k <- readLn :: IO Int
  putStrLn $ cipher s k
