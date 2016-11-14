import Control.Monad

getDist :: (Int,Int) -> (Int,Int) -> Double
getDist (x1,y1) (x2,y2) = sqrt (realToFrac $ (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))

main = do
  n <- readLn :: IO Int
  dataList <- replicateM n $ do
    input <- getLine
    let [x,y] = map (read::String->Int) $ words input
    return (x,y) 

  let calcList = (dataList!!(length dataList-1)) : dataList
  
  print.sum $ map (\x -> getDist (calcList!!(x)) (calcList!!(x+1))) [0..length calcList -2]
