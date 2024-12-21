{-# LANGUAGE QuasiQuotes #-}
module Main where
import System.Environment
import Text.RawString.QQ
import Text.Regex.PCRE
import Control.Arrow
import Data.Maybe
import Control.Monad.State
import Control.Monad

main :: IO ()
main = do
  inp <- readFile . flip (!!) 0 =<< getArgs
  let (garbage, groups) = second calcScore $ clearGarbage $ head $ lines inp
  print groups
  print garbage

calcScore :: String -> Int
calcScore groups = evalState (foldM walkGroups 0 groups) $ 0
  where
    walkGroups :: Int -> Char -> State Int Int
    walkGroups acc '{' = modify (+ 1) >> get >>= return . (+ acc)
    walkGroups acc '}' = modify (flip (-) 1) >> return acc
    walkGroups acc _ = return acc

clearGarbage :: String -> (Int, String)
clearGarbage = uncurry fromMaybe . second (fmap $ advance) <<< (,) 0 &&& localMatch
  where
    localMatch :: String -> Maybe (String, String, String)
    localMatch = flip (=~~) [r|<(\!.|[^>])*>|]
    scoreGarbage :: String -> Int
    scoreGarbage s = length s - 2 - (2 * (s =~ [r|!.|]))
    advance :: (String, String, String) -> (Int, String)
    advance (front, garbage, back) = let (extra, prunedBack) = clearGarbage back in (scoreGarbage garbage + extra, front ++ prunedBack)
