module Main where
import Data.Char
import Data.List
import Data.List.Split
import Data.Tuple.Extra
import Data.Maybe
import qualified Data.Map as Map
import System.Environment

main :: IO ()
main = do
  file <- readFile . flip (!!) 0 =<< getArgs
  let rawNums = map (read @Int) $ filter (not . null) $ splitWhen (isSpace) $ file
  let (l1, l2) = both (sort) $ unzip $ reduceNums $ rawNums
  print $ sum $ map (abs . uncurry (-)) $ zip l1 l2
  let freq_map = Map.fromListWith (+) $ map (\n -> (n, 1)) l2
  print $ sum $ map (\n -> n * (fromMaybe 0 $ Map.lookup n freq_map)) $ l1
  where
    reduceNums :: [a] -> [(a,a)]
    reduceNums (x1:x2:xs) = (x1, x2) : reduceNums xs
    reduceNums (_:_) = undefined
    reduceNums [] = []
