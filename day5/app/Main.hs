module Main where

import Data.Range
import Data.Maybe
import Data.Either
import Data.Bifunctor
import Data.List
import Text.Parsec
import Text.Parsec.String
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  let seedsP1 = parse parseSeedsP1 "" (head $ lines file)
  let seedsP2 = parse parseSeedsP2 "" (head $ lines file)
  let maps = parse (many $ parseMap >>= \x -> optional newline >> return x) "" $ (skipLine $ skipLine file)
  print $ (part1 (fromRight [] seedsP1) $ fromRight [] maps)
  print $ (part2 (fromRight [] seedsP2) $ fromRight [] maps)
  return ()
  where skipLine :: String -> String
        skipLine = drop 1 . dropWhile (not . (== '\n'))

newtype Map = Map [(Range Int, Int)]
  deriving Show

parseSeedsP1 :: Parser [Int]
parseSeedsP1 = do
  string "seeds: " >> sepBy (read <$> many1 digit) spaces

parseSeedsP2 :: Parser [Range Int]
parseSeedsP2 = do
  grp <$> parseSeedsP1
  where grp :: [Int] -> [Range Int]
        grp [] = []
        grp (_:[]) = undefined
        grp (m:n:rest) = (m +=* (m+n)) : grp rest

parseMap :: Parser Map
parseMap = do
  _ <- manyTill anyChar (try $ char ':') >> newline
  Map <$> many (parseRangePair >>= \x -> newline >> return x)
  where parseRangePair :: Parser (Range Int, Int)
        parseRangePair = do
          a <- read <$> many1 digit
          spaces
          b <- read <$> many1 digit
          spaces
          len <- read <$> many1 digit
          return (b +=* (b+len), a - b)

part1 :: [Int] -> [Map] -> Int
part1 seeds maps = do
  minimum $ foldl inner (seeds) maps
  where inner :: [Int] -> Map -> [Int]
        inner ss m = (flip convertViaMap $ m) <$> ss

part2 :: [Range Int] -> [Map] -> Int
part2 seedss ms = minimum $ map rangeStart $ foldl inner seedss ms
  where inner :: [Range Int] -> Map -> [Range Int]
        inner [] _ = []
        inner (seeds:rest) m@(Map rs) = (uncurry (:)) $ second ((flip inner $ m) . maybe rest (:rest)) $ curSet
          where curSet :: (Range Int, Maybe (Range Int))
                curSet = fromMaybe (seeds, Nothing) $ convert <$> find (rangesOverlap seeds . fst) rs
                convert :: (Range Int, Int) -> (Range Int, Maybe (Range Int))
                convert (range, offset) = first ((flip rangeTranspose $ offset) . head) $ second listToMaybe $ rangeIntersectDiff seeds range


convertViaMap :: Int -> Map -> Int
convertViaMap n (Map rs) = fromMaybe n $ ((+n) . snd) <$> (listToMaybe $ filter (flip inRange n . fst) $ rs)

rangeIntersectDiff :: Ord a => Range a -> Range a -> ([Range a], [Range a])
rangeIntersectDiff ra rb = (intsct, difference [ra] intsct)
  where intsct = intersection [ra] [rb]

rangeTranspose :: Num a => Range a -> a -> Range a
rangeTranspose (SpanRange (Bound x u) (Bound y v)) offset = SpanRange (Bound (x+offset) u) (Bound (y+offset) v)
rangeTranspose (SingletonRange n) offset = SingletonRange $ n + offset
rangeTranspose (LowerBoundRange (Bound n t)) offset = LowerBoundRange $ Bound (n+offset) t
rangeTranspose (UpperBoundRange (Bound n t)) offset = UpperBoundRange $ Bound (n+offset) t
rangeTranspose _ _ = undefined

rangeStart :: Range a -> a
rangeStart (SpanRange (Bound n _) _) = n
rangeStart (SingletonRange n) = n
rangeStart (LowerBoundRange (Bound n _)) = n
rangeStart _ = undefined

rangeEnd :: Range a -> a
rangeEnd (SpanRange _ (Bound n _)) = n
rangeEnd (SingletonRange n) = n
rangeEnd (UpperBoundRange (Bound n _)) = n
rangeEnd _ = undefined
