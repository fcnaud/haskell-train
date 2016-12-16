import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  dataList <- replicateM n (readLn::IO Int)
  mapM_ print $ sort dataList
