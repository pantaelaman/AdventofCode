module Main where
import Data.Bits (xor, shiftL, shiftR)
import Control.Arrow
import Data.List
import qualified Data.IntMap as IM

main :: IO ()
main = do
  inp <- getContents
  let secrets = read @Int <$> (filter (not . null) $ lines inp)
  let total = sum $ ((!! 2000) . iterate nextSecret) <$> secrets
  print total
  let prices = (take 2001 . fmap (`mod` 10) . iterate nextSecret) <$> secrets
  let seqs = foldr1 (IM.unionWith (+)) $ IM.fromListWith (flip const) . fmap (first hashDeltas . pricesToDelta). windows 5 <$> prices
  let optimal = maximum $ snd <$> IM.toList seqs
  print optimal
  where
    pricesToDelta :: [Int] -> ([Int], Int)
    pricesToDelta = (zipWith (flip (-)) <*> drop 1) &&& last

hashDeltas :: [Int] -> Int
hashDeltas = foldr1 $ curry $ uncurry (+) . second ((* 19) . (+ 9))

nextSecret :: Int -> Int
nextSecret n = genNum (`shiftL` 11) . genNum (`shiftR` 5) . genNum (`shiftL` 6) $ n
  where
    genNum :: (Int -> Int) -> Int -> Int
    genNum op = (`rem` 16777216) <<< xor <*> op

windows :: Int -> [Int] -> [[Int]]
windows n = foldr (zipWith (:)) (repeat []) . take n . tails
