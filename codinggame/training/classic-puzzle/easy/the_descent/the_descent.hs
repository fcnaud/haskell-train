import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    loop

loop :: IO ()
loop = do
    
    mountainData <- replicateM 8 $ do
        input_line <- getLine
        let mountainh = read input_line :: Int -- represents the height of one mountain.
        return mountainh -- change return () to return mountainh
    

    -- my solution
    
    let maxh = maximum mountainData    
        maxi = length (takeWhile (/=maxh) mountainData)
    
    putStrLn $ show maxi
    
    --

    loop