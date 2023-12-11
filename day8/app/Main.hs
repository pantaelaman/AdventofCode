module Main where

import Data.Char
import Data.Either
import Text.Parsec
import Text.Parsec.String
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy ((!))
import System.Environment

main :: IO ()
main = do
  fname <- head <$> getArgs
  file <- readFile fname
  let (dirs, network) = fromRight (undefined) $ parse readInput fname file
  print $ puzzle (cycle dirs) network (=="ZZZ") "AAA"
  let starters = filter ((=='A') . last) $ Map.keys network
  let loop_lengths = (puzzle (cycle dirs) network ((=='Z') . last)) <$> starters
  print $ foldl (lcm) (head loop_lengths) loop_lengths

type Network = Map.Map [Char] ([Char], [Char])
type Direction = ([Char], [Char]) -> [Char]

readInput :: Parser ([Direction], Network)
readInput = do
  directions <- readDirections
  _ <- newline
  network <- Map.fromList <$> many (readNetworkEntry >>= \x -> newline >> return x)
  return (directions, network)
  where readNetworkEntry :: Parser ([Char], ([Char], [Char]))
        readNetworkEntry = do
          name <- many $ satisfy isAlphaNum
          _ <- string " = ("
          fst_entry <- many $ satisfy isAlphaNum
          _ <- string ", "
          snd_entry <- many $ satisfy isAlphaNum
          _ <- string ")"
          return (name, (fst_entry, snd_entry))
        readDirections :: Parser [Direction]
        readDirections =
          manyTill (direction <$> oneOf "LR") newline
          where direction :: Char -> Direction
                direction 'L' = fst
                direction 'R' = snd
                direction _ = undefined

puzzle :: [Direction] -> Network -> ([Char] -> Bool) -> [Char] -> Int
puzzle (dir:dirs) network cond current
  | cond current = 0
  | otherwise = (+1) $ puzzle dirs network cond $ dir $ network ! current
puzzle [] _ _ _ = undefined
