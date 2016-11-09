takeSame :: String -> String -> String
takeSame [] _ = []
takeSame _ [] = []
takeSame (x1:xs1) (x2:xs2)
  | x1 == x2 = x1 : takeSame xs1 xs2
  | otherwise = []

myShow :: String -> IO ()
myShow [] = putStrLn "0"
myShow xs = putStrLn $ (show $ length xs) ++ " " ++ xs 

main = do
  str1 <- getLine
  str2 <- getLine
  let str3 = takeSame str1 str2
      len  = length str3
  myShow str3
  myShow $ drop len str1
  myShow $ drop len str2
  
