import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    input_line <- getLine
    let n = read input_line :: Int -- the number of temperatures to analyse
    temps <- getLine


    -- my solution
    let myData  = map (read::String->Int) $ words temps
    
    if length myData == 0 then putStrLn "0"
    else
        let
            minTem  = minimum $ map (\x -> abs (x-0) ) myData
            myData' = filter (\x -> abs (x-0) == minTem) myData
            result  = if any (>0) myData' then abs $ myData'!!0
                      else myData'!!0
        in
        putStrLn $ show result

    --

    return ()
