module Main where

import Data.List.Extra
import Control.Monad.State
import Control.Monad
import Data.Char
import Data.Bits (xor)
import Text.Printf

data Ring a = Ring {_ptr :: Int, _len :: Int, _contents :: [a]}

rotate :: Int -> Ring a -> Ring a
rotate n (Ring ptr len conts) = Ring newPtr len rotatedList
  where
    newPtr = (((ptr - n) `mod` len) + len) `mod` len
    rotatedList = zipWith const (drop n (cycle conts)) conts

reverseN :: Int -> Ring a -> Ring a
reverseN n (Ring ptr len conts) = Ring ptr len $ (reverse $ take n conts) ++ (drop n conts)

resetPtr :: Ring a -> Ring a
resetPtr ring@(Ring ptr _ _) = rotate ptr ring

main :: IO ()
main = do
  --inp <- readFile . flip (!!) 0 =<< getArgs
  inp <- getContents
  let lengths = read @Int <$> splitOn "," inp
  let bytes = (ord <$> inp) ++ [17, 31, 73, 47, 23]
  let ring = Ring 0 listLength $ take listLength [0..]
  let checksum = foldr1 (*) $ take 2 $ _contents $ resetPtr $ flip evalState 0 $ foldM updateRing ring $ lengths
  let sparseHash = _contents $ resetPtr $ flip evalState 0 $ runHash ring bytes
  let denseHash = foldl1 (++) $ fmap (formatHex . foldr1 xor) $ chunksOf 16 $ sparseHash
  print $ checksum
  putStrLn $ denseHash
  where
    formatHex :: Int -> String
    formatHex = printf "%02x"
    runHash :: Ring Int -> [Int] -> State Int (Ring Int)
    runHash ring = foldM updateRing ring . concat . replicate 64
    listLength :: Int = 256
    updateRing :: Ring Int -> Int -> State Int (Ring Int)
    updateRing ring revLen = do
      skipNum <- get
      modify (+ 1)
      return $ rotate (skipNum + revLen) . reverseN revLen $ ring
