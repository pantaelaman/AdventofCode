module Main where
import Control.Arrow
import Data.Char
import Data.HashMap.Strict qualified as HM
import Data.Maybe
import System.Environment
import Text.ParserCombinators.ReadP qualified as RP
import Text.ParserCombinators.ReadPrec qualified as RPr
import Text.Read

data Instruction = Instruction {reg :: String, delta :: Int, query :: String, condition :: Int -> Bool}

instance Read Instruction where
  readPrec = do
    reg <- RPr.lift $ RP.munch1 (not . isSpace)
    RPr.lift RP.skipSpaces
    dir <- RPr.lift $ (RP.string "inc" >> return id) RP.+++ (RP.string "dec" >> return negate)
    RPr.lift RP.skipSpaces
    amount <- readPrec @Int
    let delta = dir amount
    _ <- RPr.lift $ RP.string " if "
    query <- RPr.lift $ RP.munch1 (not . isSpace)
    RPr.lift RP.skipSpaces
    rawOp <- RPr.lift $ RP.munch1 (not . isSpace)
    condOp <- case rawOp of
      ">" -> return (>)
      "<" -> return (<)
      ">=" -> return (>=)
      "<=" -> return (<=)
      "==" -> return (==)
      "!=" -> return (/=)
      _ -> RPr.lift $ RP.pfail
    RPr.lift RP.skipSpaces
    condVal <- readPrec @Int
    let condition = flip condOp $ condVal
    return Instruction {reg, delta, query, condition}

main :: IO ()
main = do
  inp <- readFile . flip (!!) 0 =<< getArgs
  let instrs = read @Instruction <$> lines inp
  let (fullMax, regs) = foldl (flip runInstruction) (0, HM.empty) $ instrs
  let lastMax = maximum $ fmap snd $ HM.toList regs
  print lastMax
  print fullMax

runInstruction :: Instruction -> (Int, HM.HashMap String Int) -> (Int, HM.HashMap String Int)
runInstruction instr (prevMax, regs) = if queryResult then doInstruction else (prevMax, regs)
  where
    queryResult = (condition instr) $ (fromMaybe 0 $ regs HM.!? query instr)
    doInstruction = max prevMax &&& flip (HM.insert $ reg instr) regs $ (delta instr +) $ prevVal
      where prevVal = fromMaybe 0 $ regs HM.!? reg instr
