module Main where

import Data.List
import qualified Data.Matrix as M
import Data.Matrix ((!), Matrix)
import qualified Data.Vector as V
import Data.Function
import Data.Functor
import Control.Monad
import System.Environment

type Coord = (Int, Int)

main :: IO ()
main = do
  file <- readFile =<< head <$> getArgs
  let starmap = M.fromLists $ lines file
  let galaxies = findGalaxies starmap
  let emptyRows = findEmptyRows starmap
  let emptyCols = findEmptyCols starmap
  print $ solve 1 emptyRows emptyCols $ pairs galaxies
  print $ solve 999999 emptyRows emptyCols $ pairs galaxies
  return ()

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

solve :: Int -> [Int] -> [Int] -> [(Coord, Coord)] -> Int
solve _ _ _ [] = 0
solve expansionFactor emptyRows emptyCols (p:ps) =
  rowDiffs + colDiffs + (solve expansionFactor emptyRows emptyCols ps)
  where rowDiffs :: Int
        rowDiffs = do
          let minx = min (fst $ fst p) (fst $ snd p) 
          let maxx = max (fst $ fst p) (fst $ snd p) 
          let crossedRows = filter (\ri -> (ri < maxx) && (ri > minx)) emptyRows
          (+ (maxx - minx)) $ (* expansionFactor) $ length $ crossedRows
        colDiffs :: Int
        colDiffs = do
          let miny = min (snd $ fst p) (snd $ snd p) 
          let maxy = max (snd $ fst p) (snd $ snd p) 
          let crossedCols = filter (\ri -> (ri < maxy) && (ri > miny)) emptyCols
          (+ (maxy - miny)) $ (* expansionFactor) $ length $ crossedCols

findGalaxies :: Matrix Char -> [Coord]
findGalaxies starmap = map fst $ filter ((=='#') . snd) $ zip [(x,y) | x <- [1..(M.ncols starmap)], y <- [1..(M.nrows starmap)]] $ M.toList starmap

findEmptyRows :: Matrix Char -> [Int]
findEmptyRows starmap = map fst $ filter (all (=='.') . snd) $ zip [1..] $ map (V.toList . (flip M.getRow $ starmap)) $ [1..(M.nrows starmap)]

findEmptyCols :: Matrix Char -> [Int]
findEmptyCols starmap = map fst $ filter (all (=='.') . snd) $ zip [1..] $ map (V.toList . (flip M.getCol $ starmap)) $ [1..(M.ncols starmap)]

conjoinMonads :: Monad m => (a -> b -> c) -> m a -> m b -> m c
conjoinMonads f a b = ap (f <$> a) b
