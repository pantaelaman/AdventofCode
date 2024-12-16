module Main where
import System.Environment

main :: IO ()
main = do
  inp <- readFile . flip (!!) 0 =<< getArgs
  print ""
