module Main where
import Control.Monad.State
import Data.List.Extra
import Data.Char
import qualified Data.HashSet as HS
import Data.Bits (xor, testBit)
import Control.Monad
import Control.Arrow

data Ring a = Ring {_ptr :: Int, _len :: Int, _contents :: [a]}

rotateRing :: Int -> Ring a -> Ring a
rotateRing n (Ring ptr len conts) = Ring newPtr len rotateRingdList
  where
    newPtr = (((ptr - n) `mod` len) + len) `mod` len
    rotateRingdList = zipWith const (drop n (cycle conts)) conts

reverseRingN :: Int -> Ring a -> Ring a
reverseRingN n (Ring ptr len conts) = Ring ptr len $ (reverse $ take n conts) ++ (drop n conts)

resetRingPtr :: Ring a -> Ring a
resetRingPtr ring@(Ring ptr _ _) = rotateRing ptr ring

knotHash :: String -> [Int]
knotHash inp = do
  let bytes = (ord <$> inp) ++ [17, 31, 73, 47, 23]
  let ring = Ring 0 listLength $ take listLength [0..]
  let sparseHash = _contents $ resetRingPtr $ flip evalState 0 $ runHash ring bytes
  fmap (foldr1 xor) $ chunksOf 16 $ sparseHash
  where
    runHash :: Ring Int -> [Int] -> State Int (Ring Int)
    runHash ring = foldM updateRing ring . concat . replicate 64
    listLength :: Int = 256
    updateRing :: Ring Int -> Int -> State Int (Ring Int)
    updateRing ring revLen = do
      skipNum <- get
      modify (+ 1)
      return $ rotateRing (skipNum + revLen) . reverseRingN revLen $ ring

main :: IO ()
main = do
  inp <- getContents
  let hashPrefix = inp ++ "-"
  print $ (hashPrefix ++) . show $ 0
  let hashes = (knotHash . (hashPrefix ++) . show) <$> [0..127]
  let graph = boolsToSet $ hashToBool hashes
  let used = HS.size graph
  print $ HS.intersection graph $ rawNeighbours (0, 1)
  print used
  print $ countGroups graph

hashToBool :: [[Int]] -> [[Bool]]
hashToBool = fmap (concat . fmap (reverse . flip fmap [0..7] . testBit))

boolsToSet :: [[Bool]] -> HS.HashSet (Int, Int)
boolsToSet = HS.fromList . map fst . filter snd . concat . fmap (\(y, ts) -> zipWith (\x t -> ((x, y), t)) [0..] ts) . zip [0..]

countGroups :: HS.HashSet (Int, Int) -> Int
countGroups graph = flip evalState HS.empty $ foldM inner 0 $ HS.toList graph
  where
    inner :: Int -> (Int, Int) -> State (HS.HashSet (Int, Int)) Int
    inner acc p = do
      taken <- get
      if HS.member p taken then return acc
      else gatherNeighbours p >> return (acc + 1)
    gatherNeighbours :: (Int, Int) -> State (HS.HashSet (Int, Int)) ()
    gatherNeighbours p = do
      modify $ HS.insert p
      taken <- get
      _ <- mapM gatherNeighbours $ HS.toList $ flip HS.difference taken $ neighbours p
      return ()
      where
        neighbours :: (Int, Int) -> HS.HashSet (Int, Int)
        neighbours = HS.intersection graph . rawNeighbours

rawNeighbours :: (Int, Int) -> HS.HashSet (Int, Int)
rawNeighbours = HS.fromList . zipWith ($) neighbourFuncs . repeat
  where
    neighbourFuncs = [second (+ 1), second (flip (-) 1), first (+ 1), first (flip (-) 1)]
