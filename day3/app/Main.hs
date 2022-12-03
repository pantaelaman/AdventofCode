module Main where

import Data.List;
import Data.Maybe;
import Data.Char;
import System.IO;
import Control.Monad;

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStr "Part 1: "
  print . sum . map (scoreChar . findRepeatedInRucksack . splitRucksack) . lines $ contents
  putStr "Part 2: "
  print . sum . map (scoreChar . findRepeatedInGroup) . (Main.group 3) . lines $ contents

splitRucksack :: String -> (String, String)
splitRucksack rucksack = (p1, p2)
  where
    l = (length rucksack) `div` 2
    p1 = take l rucksack
    p2 = drop l rucksack

findRepeatedInRucksack :: (String, String) -> Char
findRepeatedInRucksack (p1, p2) = p1!!(fromJust $ elemIndex True $ (map $ flip elem p2) p1)

scoreChar :: Char -> Int
scoreChar c =
  if ord c >= 0x61 then 96 `subtract` (ord c)
  else 38 `subtract` (ord c)

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (Main.group n (drop n l))
  | otherwise = error "Negative or zero n"

findRepeatedInGroup :: [String] -> Char
findRepeatedInGroup [p1, p2, p3] = do
  let bools1 = (map $ flip elem p2) p1
  let bools2 = (map $ flip elem p3) p1
  let trueBools = map (\(x, y) -> x && y) $ zip bools1 bools2
  p1 !! fromJust (elemIndex True trueBools)

