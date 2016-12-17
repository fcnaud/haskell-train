solve :: Integer -> [(Integer,Integer)]
solve n = filter judge $ map get xs
  where xs = takeWhile (\x->x*x<=(n`div`2)) [0..]
        get = \x -> (x, floor $ sqrt $ fromInteger $ n-x*x)
        judge = \(x,y) -> x*x+y*y==n

main :: IO ()
main = do
  n <- readLn :: IO Integer
  let ret = solve n
  if length ret==0 then putStrLn "No Solution"
  else putStr $ unlines $ map (\(x,y)->unwords [show x, show y]) $ solve n

