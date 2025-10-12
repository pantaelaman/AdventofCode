{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Exit
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Control.Arrow
import Data.Either

data Disc = Disc { discNumber :: Int, discPositions :: Int, discPosition :: Int }
  deriving Show

data ModEq = ModEq { modeqOffset :: Int, modeqBase :: Int }
  deriving Show

discToModEq :: Disc -> ModEq
discToModEq (Disc n p o) = ModEq ((n + o) `rem` p) p

parseDisc :: Parsec Void String Disc
parseDisc = Disc <$>
  (string "Disc #" >> decimal) <*>
  (string " has " >> decimal) <*>
  (string " positions; at time=0, it is at position " >> decimal)
  <* string "."

bezout :: Int -> Int -> (Int, Int)
bezout a b
  | a > b = gcdEEA a b
  | otherwise = (uncurry . flip) (,) $ gcdEEA b a
  where
    gcdEEA :: Int -> Int -> (Int, Int)
    gcdEEA a b
      | nextrow == 1 = (1, coeff)
      | nextrow == 0 = undefined
      | otherwise = (f, e + f * coeff)
      where
        coeff = negate $ a `div` b
        nextrow = a `rem` b
        (e, f) = gcdEEA b nextrow

solvePair :: ModEq -> ModEq -> ModEq
solvePair (ModEq oa ba) (ModEq ob bb) = ModEq newOffset newBase
  where
    (ca, cb) = bezout ba bb
    newBase = ba * bb
    rawOffset = (`rem` newBase) $ ob * ca * ba + oa * cb * bb
    newOffset
      | rawOffset < 0 = newBase + rawOffset
      | otherwise = rawOffset

main :: IO ()
main = do
  inp <- getContents
  let discsParse = parse (many $ parseDisc <* optional newline) "inp" inp
  discs <- case discsParse of
    Left err -> (putStrLn $ errorBundlePretty err) >> exitFailure
    Right discs -> return discs

  let eqs = discToModEq <$> discs

  let (ModEq so sb) = foldr1 solvePair eqs
  let p1 = sb - so

  putStrLn $ show p1

  let p2discs = discs ++ [Disc (length discs + 1) 11 0]
  let p2eqs = discToModEq <$> p2discs
  let (ModEq so2 sb2) = foldr1 solvePair p2eqs
  let p2 = sb2 - so2

  putStrLn $ show p2

  return ()
