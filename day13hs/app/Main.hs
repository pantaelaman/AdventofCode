module Main where
import Data.List.Extra
import Data.Tuple.Extra (both, fst3, snd3, thd3)
import Control.Arrow
import Data.Maybe

main :: IO ()
main = do
  inp <- getContents
  let scanners :: [(Int, Int, Int)] = includeLength . both (read @Int) . fromJust . stripInfix ": " <$> (filter (not . null) $ lines inp)
  let severity = sum $ uncurry (*) <$> calcCaptures 0 scanners
  print severity
  let firstSafe = head $ filter (null . flip calcCaptures scanners) $ [0..]
  print firstSafe
  where
    includeLength (depth, range) = (depth, range, range * 2 - 2)
    calcCaptures :: Int -> [(Int, Int, Int)] -> [(Int, Int)]
    calcCaptures offset = fmap (fst3 &&& snd3) . filter ((== 0) . uncurry rem . first (+ offset) <<< fst3 &&& thd3)

