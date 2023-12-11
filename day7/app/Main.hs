module Main where

import Data.Char
import Data.List
import Data.List.Extra
import Data.Bool
import Data.Maybe
import Data.Either
import Data.Bifunctor
import Text.Parsec
import Text.Parsec.String
import System.Environment

type Card = Int
type Bet = Int
type Hand = [Card]
-- data HandValue = Five | Four | Full | Three | TPair | Pair | High
data HandValue = High | Pair | TPair | Three | Full | Four | Five
  deriving (Show, Eq, Ord)


main :: IO ()
main = do
  file <- (readFile . head) =<< getArgs
  let p1 = parse (parseInput charToCardP1) "" file
  let p2 = parse (parseInput charToCardP2) "" file
  print $ solve evaluateHandP1 $ fromRight [] p1
  print $ solve evaluateHandP2 $ fromRight [] p2
  return ()

solve :: (Hand -> HandValue) -> [(Hand, Bet)] -> Int
solve eh hbs = do
  foldl totalBets 0 $ zip [1..] $ map snd $ concat
    $ map (sortBy handSort) $ groupBy valueGroup $ sortBy valueSort
    $ map (first (\h -> (eh h, h))) hbs
  where valueSort :: ((HandValue, Hand), Bet) -> ((HandValue, Hand), Bet) -> Ordering
        valueSort ((v1, _), _) ((v2, _), _) = compare v1 v2 
        valueGroup :: ((HandValue, Hand), Bet) -> ((HandValue, Hand), Bet) -> Bool
        valueGroup ((v1, _), _) ((v2, _), _) = (==) v1 v2
        handSort :: ((HandValue, Hand), Bet) -> ((HandValue, Hand), Bet) -> Ordering
        handSort ((_, h1), _) ((_, h2), _) = compare h1 h2
        totalBets :: Int -> (Int, Bet) -> Int
        totalBets acc (n, b) = acc + (n*b)

collapseTuple :: Monad m => (m a, b) -> m (a, b)
collapseTuple (a, b) = a >>= (\x -> return (x, b))

parseInput :: (Char -> Card) -> Parser [(Hand, Bet)]
parseInput ctc = manyTill (parseHandBet >>= \x -> newline >> return x) eof
  where parseHandBet :: Parser (Hand, Bet)
        parseHandBet = do
          hand <- map ctc <$> manyTill anyChar (try space)
          bet <- read <$> many1 digit
          return (hand, bet)

charToCardP1 :: Char -> Card
charToCardP1 c
  | c == 'A' = 14
  | c == 'K' = 13
  | c == 'Q' = 12
  | c == 'J' = 11
  | c == 'T' = 10
  | otherwise = (ord c) - 0x30

charToCardP2 :: Char -> Card
charToCardP2 c
  | c == 'A' = 14
  | c == 'K' = 13
  | c == 'Q' = 12
  | c == 'J' = 1
  | c == 'T' = 10
  | otherwise = (ord c) - 0x30

evaluateHandP1 :: Hand -> HandValue
evaluateHandP1 hand
  | num_uniques == 1 = Five
  | num_uniques == 2 = bool Full Four $ isJust $ find (\(_,c) -> c == 1) grouped 
  | num_uniques == 3 = bool TPair Three $ isJust $ find (\(_,c) -> c == 3) grouped
  | num_uniques == 4 = Pair
  | otherwise = High
  where grouped :: [(Card, Int)] = map (\x -> (head x, length x)) $ group $ sort hand
        num_uniques :: Int = length grouped

evaluateHandP2 :: Hand -> HandValue
evaluateHandP2 hand = do
  let grpd = grouped
  let nu = length grpd
  if nu == 1 then Five
  else if nu == 2 then bool Full Four $ isJust $ find (\(_,c) -> c == 1) $ grpd 
  else if nu == 3 then bool TPair Three $ isJust $ find (\(_,c) -> c == 3) $ grpd
  else if nu == 4 then Pair
  else High
  where grouped :: [(Card, Int)]
        grouped = do
          let groups = sortBy (\(_, c1) (_, c2) -> compare c1 c2) $ map (\x -> (head x, length x)) $ group $ sort hand
          let jokers = fromMaybe 0 (snd <$> find ((==1) . fst) groups)
          maybe [(1, 5)] (uncurry snoc . second (second (+jokers))) $ unsnoc $ filter ((/=1) . fst) groups

showHand :: Hand -> [Char]
showHand hand = numToChar <$> hand
  where numToChar :: Int -> Char
        numToChar n
          | n == 1 = 'J'
          | n == 10 = 'T'
          | n == 11 = 'J'
          | n == 12 = 'Q'
          | n == 13 = 'K'
          | n == 14 = 'A'
          | otherwise = chr $ n + 0x30
