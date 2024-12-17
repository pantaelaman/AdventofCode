module Main where
import System.Environment
import Data.Tuple.Extra
import Data.List.Split
import Data.Char

main :: IO ()
main = do
  inp <- readFile . flip (!!) 0 =<< getArgs
  let reports = map parseReport $ lines inp
  let safeReports = filter safeReport $ reports
  print $ length safeReports
  let addlSafe = filter checkRemoved $ reports
  print $ length addlSafe
  where
    parseReport :: [Char] -> [Int]
    parseReport str = map (read @Int) $ filter (not . null) $ splitWhen isSpace $ str

checkRemoved :: [Int] -> Bool
checkRemoved report = any safeReport $ map (strippedReport report) $ [0..length report]
  where
    strippedReport :: [Int] -> Int -> [Int]
    strippedReport rp n = take n rp ++ drop (n+1) rp

safeReport :: [Int] -> Bool
safeReport report =
  uncurry (&&) $ (allEq . map signum) &&& (all inRange) $ map (uncurry (-)) $ (zip <*> drop 1) report
  where
    inRange :: Int -> Bool
    inRange n = n <= 3 && n >= -3 && n /= 0
    allEq :: [Int] -> Bool
    allEq = all (uncurry (==)) . (zip <*> drop 1)

