import System.IO
import Control.Monad
import Data.Char

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    input_line <- getLine
    let l = read input_line :: Int
    input_line <- getLine
    let h = read input_line :: Int
    t <- getLine
    
    let needs = map trans t
    
    inputData <- replicateM h $ do
        row <- getLine
        return row
    
    
    let ans = unlines $ map (getAnsLine inputData needs l) [0..h-1]
    
    putStrLn ans
    return ()
    
trans :: Char -> Int
trans ch
    | isUpper ch = ord ch - ord 'A'
    | isLower ch = ord ch - ord 'a'
    | otherwise  = 26

getAnsLine :: [String] -> [Int] -> Int -> Int -> String    
getAnsLine inputData needs l x = concatMap getWordLine needs
    where getWordLine y = take l $ drop (l*y) (inputData!!x)

