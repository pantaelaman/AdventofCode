module Main where

import Data.List
import Data.Bool
import Data.Maybe
import Data.Tuple.Extra
import qualified Data.Matrix as M
import Data.Matrix ((!))
import System.Environment

type Coord = (Int, Int)

neighbours :: [Coord]
neighbours = [(0, -1), (-1, 0), (0, 1), (1, 0)]
viableFrom :: [[Char]]
viableFrom = ["L-F", "F|7", "7-J", "J|L"]

main :: IO ()
main = do
  file <- readFile =<< head <$> getArgs
  let pgrid = M.fromLists $ lines file
  let pgrid_width = M.ncols pgrid
  let start = ((+1) . (`quot` pgrid_width)) *** ((+1) . (`rem` pgrid_width)) $ dupe $ fromJust $ elemIndex 'S' $ M.toList pgrid
  let firstStep = findFirst pgrid start
  let path = start : solveLoop pgrid start start firstStep
  print $ (length path) `quot` 2
  print $ solveSurrounded pgrid path ((`elem` "L|J") $ getS pgrid start)
  return ()

solveSurrounded :: M.Matrix Char -> [Coord] -> Bool -> Int
solveSurrounded pgrid loop includeS = sum $ map ((flip $ curry $ inner False) 0) $ [1..(M.nrows pgrid)]
  where inner :: Bool -> Coord -> Int
        inner isInside crd
          | snd crd > M.ncols pgrid = 0
          | elem crd loop =
            inner ((/= isInside) . (\c -> (elem c "L|J") || (includeS && c == 'S')) $ (pgrid ! crd)) (second (+1) $ crd)
          | otherwise =
            (bool 0 1 isInside) + (inner isInside $ second (+1) crd) -- do truth table for == isInside

solveLoop :: M.Matrix Char -> Coord -> Coord -> Coord -> [Coord]
solveLoop pgrid start prev current
  | current == start = []
  | otherwise = do
      let c = pgrid ! current
      let next = getNext prev current c
      current : solveLoop pgrid start current next

getNext :: Coord -> Coord -> Char -> Coord
getNext from current c = do
  let change = diffCoords current from
  head $ map (addCoords current . invertCoord) $ filter (/= change) $ map snd $ filter (elem c . fst) $ zip viableFrom neighbours

findFirst :: M.Matrix Char -> Coord -> Coord
findFirst pgrid start =
  snd $ snd $ head $ filter (\(vs, (c, _)) -> elem c vs) $ mapMaybe (expandMonadic . second (first ((flip $ uncurry M.safeGet) $ pgrid) . dupe . addCoords start)) $ zip viableFrom neighbours

getS :: M.Matrix Char -> Coord -> Char
getS pgrid start = do
  let vneighbours = map fst $ filter (\(vs, (c, _)) -> elem c vs) $ mapMaybe (expandMonadic . second (first ((flip $ uncurry M.safeGet) $ pgrid) . dupe . addCoords start)) $ zip viableFrom neighbours
  let (a, b) = second head $ fromJust $ uncons $ filter (not . (flip elem $ vneighbours)) viableFrom
  head $ filter (\x -> elem x b) a

addCoords :: Coord -> Coord -> Coord
addCoords (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

diffCoords :: Coord -> Coord -> Coord
diffCoords (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

invertCoord :: Coord -> Coord
invertCoord = both negate

expandMonadic :: Monad m => (a, (m b, c)) -> m (a, (b, c))
expandMonadic (a, (b, c)) = b >>= \x -> return (a, (x, c))
