import Data.Char
import Control.Monad

getProduct :: [Int] -> Int -> Int -> Int
getProduct myData m n = product [myData!!(n+x)|x<-[0..m-1]]

main = do
  n <- readLn :: IO Int
  ansList <- replicateM n $ do
    input <- getLine
    str   <- getLine
    let [x, y] = map (read::String->Int) $ words input
    let numList = map digitToInt str
    let ans = maximum $ map (getProduct numList y) [0..length numList - y]
    return ans

  mapM_ (putStrLn.show) ansList

