import Data.Char

isFlower :: Int -> Bool
isFlower num = num == (sum $ map (flip (^) n) xs)
  where xs = map digitToInt $ show num
        n = length xs

flowers :: [Int]
flowers = [x| x<-[100..], isFlower x]

main :: IO ()
main = do
  n <- readLn :: IO Int
  print $ head $ dropWhile (<n) flowers


