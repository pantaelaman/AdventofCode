module Main where

import System.Environment
import Text.Parsec
import Text.Parsec.String
import Data.Either

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  let cards = sequence $ map (parse parseCard "") $ lines file
  print $ sum $ fromRight [] $ map (scoreCard) <$> cards
  print $ cardCopies $ fromRight [] cards

-- Card winners numbers
data Card = Card [Int] [Int]
  deriving Show

parseCard :: Parser Card
parseCard = do
  string "Card" >> spaces >> many1 digit >> char ':' >> spaces
  winners <- manyTill number $ char '|'
  spaces
  numbers <- many number
  return $ Card winners numbers
  where number :: Parser Int
        number = do
          n <- many1 digit
          spaces
          return $ read n

scoreCard :: Card -> Int
scoreCard card
  | n == 0 = 0
  | otherwise = 2^(n - 1)
  where n = numWinners card

numWinners :: Card -> Int
numWinners (Card winners numbers) = length $ filter (id) $ map (flip elem winners) numbers

cardCopies :: [Card] -> Int
cardCopies cards = inner cards $ replicate (length cards) 1  
  where inner :: [Card] -> [Int] -> Int
        inner (c:cs) (n:ns) = do
          n + (inner cs applyCopies)
          where score :: Int = numWinners c
                applyCopies :: [Int]
                applyCopies = (map (n +) $ take score ns) ++ drop score ns
        inner _ [] = 0
        inner [] _ = 0
