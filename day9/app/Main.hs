module Main where

import Data.Either
import Text.Parsec
import Text.Parsec.String
import System.Environment

main :: IO ()
main = do
  file <- readFile =<< head <$> getArgs
  let parsed = fromRight (undefined) $ parse readInput "" file
  print $ (sum . map part1) $ parsed
  print $ (sum . map part2) $ parsed

diffs :: [Int] -> [Int]
diffs (a:b:s) = (b-a) : diffs (b:s)
diffs _ = []

readInput :: Parser [[Int]]
readInput = many1 readLine
  where readLine :: Parser [Int]
        readLine = manyTill (optional (char ' ') >> read <$> (many1 $ oneOf "-1234567890")) newline

part1 :: [Int] -> Int
part1 nums
  | all (==0) nums = 0
  | otherwise = (+ last nums) $ part1 $ diffs nums

part2 :: [Int] -> Int
part2 nums
  | all (==0) nums = 0
  | otherwise = ((-) $ head nums) $ part2 $ diffs nums
