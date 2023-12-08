module Main where

import Data.Either
import Text.Parsec
import Text.Parsec.String
import System.Environment

data Race = Race Int Int
  deriving Show

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  let races = parse parseRaces "" file
  let race = parse parseRace "" file
  print $ product $ (solveRace <$> fromRight [] races)
  print $ fromRight (-1) $ solveRace <$> race

parseRaces :: Parser [Race]
parseRaces = do
  _ <- string "Time:"
  ts <- map read <$> manyTill (spaces >> many1 digit) newline
  _ <- string "Distance:"
  ds <- map read <$> manyTill (spaces >> many1 digit) newline
  return $ (\(t, d) -> Race t d) <$> zip ts ds

parseRace :: Parser Race
parseRace = do
  _ <- string "Time:"
  t <- read . concat <$> manyTill (spaces >> many1 digit) newline
  _ <- string "Distance:"
  d <- read . concat <$> manyTill (spaces >> many1 digit) newline
  return $ Race t d

solveRace :: Race -> Int
solveRace (Race t d) = top - bottom
  where part :: Double
        part = (/2) $ sqrt $ fromIntegral (t^2 - 4*d)
        half_time :: Double = (/2) $ fromIntegral t
        top :: Int = ceiling $ (half_time + part)
        bottom :: Int
          | truncate n == ceiling n = (+1) $ truncate n
          | otherwise = ceiling n
          where n :: Double = half_time - part
