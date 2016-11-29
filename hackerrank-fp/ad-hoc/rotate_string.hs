import Control.Monad

main :: IO()
main = do
  n <- readLn :: IO Int
  replicateM_ n $ do
    str <- getLine
    putStrLn.unwords $ map (\m -> (drop m str) ++ (take m str)) [1..length str]
  


