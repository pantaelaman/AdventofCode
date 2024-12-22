module Main where
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List.Extra
import Data.Maybe
import Control.Monad.State
import Control.Monad

main :: IO ()
main = do
  inp <- getContents
  let mapping = IM.fromList $ parseLine <$> (filter (not . null) $ lines inp)
  let group0 = gatherGroup 0 mapping
  print $ IS.size group0
  let total = totalGroups mapping
  print total
  where
    parseLine :: String -> (Int, [Int])
    parseLine = fromJust . uncons . fmap (read @Int) . concat . fmap (splitOn ", ") . splitOn " <-> "

gatherGroup :: Int -> IM.IntMap [Int] -> IS.IntSet
gatherGroup n mapping = inner n IS.empty
  where
    inner :: Int -> IS.IntSet -> IS.IntSet
    inner ln set
      | IS.member ln set = set
      | otherwise = foldr (inner) (IS.insert ln set) $ mapping IM.! ln

totalGroups :: IM.IntMap [Int] -> Int
totalGroups mapping = flip evalState IS.empty $ foldM inner 0 $ fst <$> IM.toList mapping
  where
    inner :: Int -> Int -> State IS.IntSet Int
    inner c n = do
      taken <- get
      if IS.member n taken then return c else
        modify (IS.union $ gatherGroup n mapping) >> return (c + 1)
