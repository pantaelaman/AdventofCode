import System.Environment
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char
import Control.Monad
import Data.Char

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  let ms = sequence $ map (maxima . snd <$>) $ map (parse gameParser "") $ lines file
  print $ part1 <$> ms
  print $ part2 <$> ms

part1 :: [RGB] -> Integer
part1 = sum . fst . unzip . filter (\x -> checkRGB $ snd x) . zip [1..]
  where checkRGB :: RGB -> Bool
        checkRGB (r, g, b) = r <= 12 && g <= 13 && b <= 14

part2 :: [RGB] -> Integer
part2 = sum . map powerRGB
  where powerRGB :: RGB -> Integer
        powerRGB (r, g, b) = r * g * b

type RGB = (Integer, Integer, Integer)

maxima :: [Showing] -> RGB
maxima showings = foldl inner (0, 0, 0) showings
  where inner :: RGB -> Showing -> RGB
        inner (r, g, b) (Showing c n)
          | c == Red = (max r n, g, b)
          | c == Green = (r, max g n, b)
          | c == Blue = (r, g, max b n)

data Colour = Red | Green | Blue
  deriving (Eq, Show)
 
data Showing = Showing Colour Integer
  deriving Show

type Game = (Integer, [Showing])

gameParser :: Parser Game
gameParser = do
  void $ string "Game "
  ncs <- many1 digit
  let n = read ncs
  char ':' >> spaces
  showings <- flip sepBy (satisfy isPunctuation >> spaces) showingParser
  return (n, showings)

showingParser :: Parser Showing
showingParser = do
  ncs <- many1 digit
  let n = read ncs
  spaces
  ccs <- many1 (satisfy isAlpha)
  let c = case ccs of 
        "red" -> Red
        "green" -> Green
        "blue" -> Blue
  return $ Showing c n
