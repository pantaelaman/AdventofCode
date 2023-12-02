{-# LANGUAGE PatternGuards #-}

import Data.Char
import Data.List
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  print $ sum $ map getNum $ lines file
  print $ sum $ map (getNum . replaceNumbers) $ lines file

getNum :: [Char] -> Integer
getNum = read . firstAndLast . filter isDigit

firstAndLast :: [a] -> [a]
firstAndLast [] = []
firstAndLast (x:[]) = x : [x]
firstAndLast (x:xs) = x : [last xs]

replaceNumbers :: [Char] -> [Char]
replaceNumbers str = reverse $ snd $ foldl (inner) ([], []) str
  where inner :: ([Char], [Char]) -> Char -> ([Char], [Char])
        inner (buf, acc) c
          | Just cs <- stripPrefix "eno" newbuf = (newbuf, '1':acc)
          | Just cs <- stripPrefix "owt" newbuf = (newbuf, '2':acc)
          | Just cs <- stripPrefix "eerht" newbuf = (newbuf, '3':acc)
          | Just cs <- stripPrefix "ruof" newbuf = (newbuf, '4':acc)
          | Just cs <- stripPrefix "evif" newbuf = (newbuf, '5':acc)
          | Just cs <- stripPrefix "xis" newbuf = (newbuf, '6':acc)
          | Just cs <- stripPrefix "neves" newbuf = (newbuf, '7':acc)
          | Just cs <- stripPrefix "thgie" newbuf = (newbuf, '8':acc)
          | Just cs <- stripPrefix "enin" newbuf = (newbuf, '9':acc)
          | isDigit c = (newbuf, c:acc)
          | otherwise = (newbuf, acc)
          where newbuf = c:buf
