import Control.Monad
import Data.List

judge :: Int -> Bool
judge n = 
  let s = show n 
  in s == reverse s

cmp :: (Ord a) => a -> a -> Ordering
cmp a b 
  | a >  b = LT
  | a <  b = GT
  | otherwise = EQ

lists :: [Int]
lists = sort $ nub [x*y|x<-[1..999], y<-[1..999], judge (x*y)]

solve :: Int -> Int
solve n = last $ takeWhile (<n) lists

main = do
  n <- readLn :: IO Int
  dataList <- replicateM n $ do
    input <- getLine
    let num = read input :: Int
    return num

  mapM_ (putStrLn.show) $ map solve dataList
