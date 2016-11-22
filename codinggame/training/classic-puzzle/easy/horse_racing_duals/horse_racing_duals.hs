import System.IO
import Control.Monad
import Data.List

main :: IO()
main = do
  hSetBuffering stdout NoBuffering

  n <- readLn :: IO Int

  dataInput <- replicateM n $ do readLn :: IO Int

  let xs  = sort dataInput
  let xs' = map abs $ zipWith (-) (init xs) (tail xs)

  putStrLn.show $ min (abs $ dataInput!!0 - dataInput!!1) (minimum xs')
