module Main where
import System.Environment
import Data.List.Split
import qualified Data.HashMap.Lazy as HM
import Control.Monad.State
import Data.List
import Data.Maybe
import Control.Monad.Cont

main :: IO ()
main = do
  inp <- readFile . flip (!!) 0 =<< getArgs
  let patterns = splitOn ", "  $ head $ lines inp
  let towels = drop 2 $ lines inp
  let counts = flip evalState HM.empty $ mapM (dfsPatterns patterns) $ towels
  let (p1, p2) = foldr (\n (p1, p2) -> (p1 + signum n, p2 + n)) (0, 0) $ counts
  print p1
  print p2

dfsPatterns :: [String] -> String -> State (HM.HashMap String Int) Int
dfsPatterns patterns = evalContT . inner
  where
    inner :: String -> ContT Int (State (HM.HashMap String Int)) Int
    inner "" = return 1
    inner haystack = do
      cache <- get
      fromMaybe uncached $ return <$> HM.lookup haystack cache
      where
        uncached :: ContT Int (State (HM.HashMap String Int)) Int
        uncached = do
          val <- fmap sum $ mapM checkPattern $ patterns
          modify $ HM.insert haystack val
          return val
        checkPattern :: String -> ContT Int (State (HM.HashMap String Int)) Int
        checkPattern pat = if pat `isPrefixOf` haystack
          then inner $ drop (length pat) haystack
          else return 0
