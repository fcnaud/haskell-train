import Control.Monad

check :: [(Int,Int)] -> Bool
check [] = True
check ((x,y):xs) = case flag of
  True  -> False
  False -> check xs''
  where flag = any (\(_,y1)->y/=y1) xs' 
        xs'  = filter (\(x1,_)->x1==x) xs
        xs'' = filter (\(x1,_)->x1/=x) xs

solve :: IO()
solve = do
  n <- readLn :: IO Int
  dataList <- replicateM n $ do
    input <- getLine
    let [x,y] = map (read::String->Int) $ words input
    return (x,y)

  if check dataList then putStrLn "YES"
  else putStrLn "NO"

main = do
  t <- readLn ::IO Int
  replicateM_ t solve
