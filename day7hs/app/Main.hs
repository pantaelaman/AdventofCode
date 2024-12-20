{-# LANGUAGE QuasiQuotes #-}
module Main where
import System.Environment
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Text.RawString.QQ
import Text.Regex.PCRE
import Control.Monad.State.Lazy
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra

type WeightMap = HashMap String Int
type ChildrenMap = HashMap String [String]

main :: IO ()
main = do
  inp <- readFile . flip (!!) 0 =<< getArgs
  (weightMap, childrenMap) <- foldr (buildMaps) (HM.empty, HM.empty) <$> (mapM lineRegex $ lines inp)
  let totalWeightMap = calcTotalWeights weightMap childrenMap
  let root = fst $ maximumBy (curry (uncurry compare . both snd)) $ HM.toList totalWeightMap
  print $ root
  let newWeight = seekImbalance totalWeightMap childrenMap root
  print $ newWeight
  where
    lineRegex :: String -> IO (String, Int, [String])
    lineRegex = fmap (makeTuples . drop 1 . head) . return . (flip (=~) regex)
      where
        regex = [r|(\w+) \((\d+)\)(?: -> ((?:(?:\w+), )*(?:\w+)))?|]
    makeTuples :: [String] -> (String, Int, [String])
    makeTuples (name:weight:children:[]) = (name, read weight, filter (not . null) $ splitOn ", " $ children)
    makeTuples _ = undefined
    buildMaps :: (String, Int, [String]) -> (WeightMap, ChildrenMap) -> (WeightMap, ChildrenMap)
    buildMaps (name, weight, children) (weightMap, childrenMap) =
      (HM.insert name weight weightMap, HM.insert name children childrenMap)

seekImbalance :: WeightMap -> ChildrenMap -> String -> Int
seekImbalance totalWeightMap children = inner 0
  where
    inner :: Int -> String -> Int
    inner expected root = case childrenWeights of
      (imbalance:balanced:[]) ->
        let newExpected = snd $ head $ balanced
            newRoot = fst $ head $ imbalance in
            inner newExpected newRoot
      weights -> expected - (sum $ map (snd) $ concat weights)
      where
        childrenWeights = sortOn length $ groupSortOn snd $ map (second (totalWeightMap HM.!) . dupe) $ children HM.! root

calcTotalWeights :: WeightMap -> ChildrenMap -> WeightMap
calcTotalWeights weightMap childrenMap = flip execState HM.empty $ mapM calcTotalWeight $ HM.keys weightMap
  where
    calcTotalWeight :: String -> State WeightMap Int
    calcTotalWeight name = do
      totalWeightMap <- get
      case HM.lookup name totalWeightMap of
        Just v -> return v
        Nothing -> do
          childWeights <- fmap sum $ mapM (calcTotalWeight) $ fromMaybe [] $ HM.lookup name childrenMap
          let totalWeight = childWeights + weightMap HM.! name
          put $ HM.insert name totalWeight totalWeightMap
          return totalWeight
