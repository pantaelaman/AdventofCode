module Main where

import System.IO
import Control.Monad

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStr "Part 1: "
  print . sum . map scoreMatchP1 . lines $ contents
  putStr "Part 2: "
  print . sum . map scoreMatchP2 . lines $ contents


scoreMatchP1 :: String -> Int
scoreMatchP1 match = 
  if match == "A X" then 4
  else if match == "B X" then 1
  else if match == "C X" then 7
  else if match == "A Y" then 8
  else if match == "B Y" then 5
  else if match == "C Y" then 2
  else if match == "A Z" then 3
  else if match == "B Z" then 9
  else if match == "C Z" then 6
  else 0 -- invalid score
    
scoreMatchP2 :: String -> Int
scoreMatchP2 match =
  if match == "A X" then 3 -- plays scissors
  else if match == "B X" then 1 -- plays rock
  else if match == "C X" then 2 -- plays paper
  else if match == "A Y" then 4 -- plays rock
  else if match == "B Y" then 5 -- plays paper
  else if match == "C Y" then 6 -- plays scissors
  else if match == "A Z" then 8 -- plays paper
  else if match == "B Z" then 9 -- plays scissors
  else if match == "C Z" then 7 -- plays rock
  else 0 -- invalid score

