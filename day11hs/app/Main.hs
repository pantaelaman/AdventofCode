module Main where
import Text.Read
import qualified Text.ParserCombinators.ReadPrec as RPr
import qualified Text.ParserCombinators.ReadP as RP
import Control.Applicative
import Data.List.Extra (splitOn)

data HexP a = HexP { x :: a, y :: a }

instance Num a => Num (HexP a) where
  (HexP x1 y1) + (HexP x2 y2) = HexP (x1 + x2) (y1 + y2)
  (HexP x1 y1) - (HexP x2 y2) = HexP (x1 - x2) (y1 - y2)
  (HexP x1 y1) * (HexP x2 y2) = HexP (x1 * x2) (y1 * y2)
  abs p = abs <$> p
  signum p = signum <$> p
  negate p = negate <$> p
  fromInteger i = HexP (fromInteger i) (fromInteger i)

instance Functor HexP where
  fmap f (HexP x1 y1) = HexP (f x1) (f y1)

instance Read (HexP Int) where
  readPrec =
    RPr.lift $ (HexP (1) (-1) <$ RP.string "ne") <|>
      (HexP (-1) (0) <$ RP.string "nw") <|>
      (HexP (1) (0) <$ RP.string "se") <|>
      (HexP (-1) (1) <$ RP.string "sw") <|>
      (HexP (0) (-1) <$ RP.string "n") <|>
      (HexP (0) (1) <$ RP.string "s")

hexMagnitude :: (Num a, Integral a) => HexP a -> a
hexMagnitude (HexP x y) = (abs x + abs (x + y) + abs y) `div` fromInteger 2

main :: IO ()
main = do
  inp <- getContents
  let moves = read @(HexP Int) <$> (filter (not . null) $ splitOn "," inp)
  let (farthest, target) = foldl trackMoves (0, HexP 0 0) $ moves
  print $ hexMagnitude target
  print $ farthest
  where
    trackMoves :: (Num a, Integral a) => (a, HexP a) -> HexP a -> (a, HexP a)
    trackMoves (prevDist, origin) delta = (max prevDist $ hexMagnitude point, point)
      where
        point = origin + delta
