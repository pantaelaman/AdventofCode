module Main where

import Control.Arrow
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Crypto.Hash

md5hash :: String -> String
md5hash = BS.unpack . digestToHexByteString . (hash :: BS.ByteString -> Digest MD5) . BS.pack

hashes :: String -> [(Int, String)]
hashes salt = (id &&& (md5hash . (salt ++) . show)) <$> [0 :: Int ..]

hashes2016 :: String -> [(Int, String)]
hashes2016 salt = (id &&& (hashing . (salt ++) . show)) <$> [0 :: Int ..]
  where
    hashing = BS.unpack . (!! 2017) . hash' . BS.pack
    hash' :: BS.ByteString -> [BS.ByteString]
    hash' = iterate (digestToHexByteString . (hash :: BS.ByteString -> Digest MD5))

seek3peat :: Eq a => [a] -> Maybe a
seek3peat (x:y:z:xs)
  | x == y && y == z = Just x
  | y == z = seek3peat $ y:z:xs
  | otherwise = seek3peat $ z:xs
seek3peat _ = Nothing

has5peat :: Eq a => a -> [a] -> Bool
has5peat x xs
  | length xs < 5 = False
  | otherwise = all (==x) (take 5 xs) || has5peat x (tail xs)

filterUnmatchedKeys :: [(Int, String)] -> [(Int, String)]
filterUnmatchedKeys = filter (isJust . seek3peat . snd)

filterKeys :: [(Int, String)] -> [(Int, String)]
filterKeys = mconcat . fmap reverse . unfoldr (next) . ((,) [])
  where
    next :: ([(Char,(Int,String))], [(Int,String)]) -> Maybe ([(Int,String)], ([(Char,(Int,String))], [(Int,String)]))
    next (_, []) = Nothing
    next (pending, (curi,curhash):hs) = Just (matchedPending, (nextState, hs))
      where
        debugInfo = do
          putStrLn $ show curi ++ " : " ++ curhash
          putStrLn $ "  " ++ show (length pending) ++ "," ++ show (length culledPending)
          putStrLn $ show matchedPending
          --mapM_ ((>> putStr ",") . putStr . show . (id &&& flip has5peat curhash) . fst) pending
          --putStrLn ""
        nextState = case seek3peat curhash of
          Just peat -> (peat, (curi,curhash)) : culledPending
          Nothing -> culledPending
        (matchedPending, culledPending) = first (map snd) . partition (\(c, _) -> has5peat c curhash) $ filter ((<= 1000) . (curi -) . fst . snd) pending

main :: IO ()
main = do
  salt <- getContents
  let keys = take 64 . filterKeys $ hashes salt
  mapM_ printKey keys
  putStrLn $ "\n\n"
  let truekey = fst $ last keys
  putStrLn $ "Part 1: " ++ show truekey
  putStrLn $ "\n\n"

  let keys = take 64 . filterKeys $ hashes2016 salt
  mapM_ printKey keys
  putStrLn $ "\n\n"
  let truekey = fst $ last keys
  putStrLn $ "Part 2: " ++ show truekey
  where
    printKey (keyi, keyhash) = do
      putStrLn $ show keyi ++ " : " ++ keyhash
