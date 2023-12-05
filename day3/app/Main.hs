module Main where

import System.Environment
import Data.Matrix hiding ((<|>))
import Data.Char
import Data.Maybe
import Data.Bool
import Control.Applicative
import qualified Data.Map.Strict as Map
import Text.Read

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  let grid :: Matrix Char = fromLists $ lines file
  -- print $ checkNeighbours 4 4 grid
  -- putStrLn $ prettyMatrix $ grid
  print $ sum $ getPartNumbers grid
  print $ sum $ getGearRatios grid

neighbours :: [(Int, Int)]
neighbours = [(x, y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]

getPartNumbers :: Matrix Char -> [Int]
getPartNumbers m = inner 1 1 False []
  where width :: Int = ncols m
        height :: Int = nrows m
        inner :: Int -> Int -> Bool -> [Char] -> [Int]
        inner x y state buf
          | x > width = (bool 0 confirmBuf state) : inner 1 (y+1) False []
          | y > height = [bool 0 confirmBuf state]
          | length buf > 0 && (not $ isDigit c) = (bool 0 confirmBuf state) : inner (x+1) y False []
          | isDigit c = inner (x+1) y (state || checkNeighbours) (c:buf)
          | otherwise = inner (x+1) y False buf
          where c :: Char = m ! (y, x)
                confirmBuf :: Int
                confirmBuf = maybe 0 id $ readMaybe $ reverse $ buf
                checkNeighbours :: Bool
                checkNeighbours =
                  foldl (||) False $ map isPart $ catMaybes $ map ((flip $ uncurry safeGet) m) $ map (sumTuple y x) $ neighbours
                  where isPart :: Char -> Bool
                        isPart p = p /= '.' && (not $ isDigit p)

getGearRatios :: Matrix Char -> [Int]
getGearRatios m = map product $ filter lengthFilter $ Map.elems $ getMap
  where getMap :: Map.Map (Int, Int) [Int] = inner 1 1 Nothing []
        lengthFilter :: [Int] -> Bool
        lengthFilter = ((==) 2) . length
        width :: Int = ncols m
        height :: Int = nrows m
        inner :: Int -> Int -> Maybe (Int, Int) -> [Char] -> Map.Map (Int, Int) [Int]
        inner x y state buf
          | x > width = finishNum state confirmBuf nextY
          | y > height = fromMaybe Map.empty (confirmBuf >>= (\n -> flip Map.singleton [n] <$> state))
          | length buf > 0 && (not $ isDigit c) = finishNum state confirmBuf (inner (x+1) y Nothing []) 
          | isDigit c = inner (x+1) y (state <|> checkNeighbours) (c:buf)
          | otherwise = inner (x+1) y Nothing []
          where c :: Char = m ! (y, x)
                nextY :: Map.Map (Int, Int) [Int]
                nextY = inner 1 (y+1) Nothing []
                finishNum :: Maybe (Int, Int) -> Maybe Int -> Map.Map (Int, Int) [Int] -> Map.Map (Int, Int) [Int]
                finishNum mcoord mn next = fromMaybe next (mn >>= (\n -> flip (flip insertIntoMap n) next <$> mcoord))
                insertIntoMap :: (Int, Int) -> Int -> Map.Map (Int, Int) [Int] -> Map.Map (Int, Int) [Int]
                insertIntoMap coord n prev = Map.insertWith (++) coord [n] prev
                confirmBuf :: Maybe Int
                confirmBuf = readMaybe $ reverse $ buf
                checkNeighbours :: Maybe (Int, Int)
                checkNeighbours =
                  listToMaybe $ map (fst) $ filter isGear $ map packNeighbour $ map (sumTuple y x) $ neighbours
                  where isGear :: (w, Maybe Char) -> Bool
                        isGear (_, Just '*') = True
                        isGear _ = False
                        packNeighbour :: (Int, Int) -> ((Int, Int), Maybe Char)
                        packNeighbour coord = (coord, (flip $ uncurry safeGet) m $ coord)

sumTuple :: Int -> Int -> (Int, Int) -> (Int, Int)
sumTuple x1 y1 (x2, y2) = (x1+x2, y1+y2)
