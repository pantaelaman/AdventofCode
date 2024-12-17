module Main where

import Data.Bits
import Data.List
import Data.Maybe
import System.Environment
import Text.Regex.PCRE

data Reg = A | B | C
  deriving (Show)

data ComboOp = Lit Int | Ptr Reg
  deriving (Show)

data Instr
  = Adv ComboOp
  | Bxl Int
  | Bst ComboOp
  | Jnz Int
  | Bxc
  | Out ComboOp
  | Bdv ComboOp
  | Cdv ComboOp
  deriving (Show)

main :: IO ()
main = do
  inp <- readFile . flip (!!) 0 =<< getArgs
  let ds = map (read @Int . flip (!!) 0) $ (inp =~ "\\d+")
  let a = head ds
  let rawProg = drop 3 ds
  let program = parseInp rawProg
  print $ runProg program a
  print $ fromJust $ dfsQuine program rawProg
  where
    parseInp :: [Int] -> [Instr]
    parseInp (n1 : n2 : inp) = instr : parseInp inp
      where
        instr = case n1 of
          0 -> Adv op
          1 -> Bxl n2
          2 -> Bst op
          3 -> Jnz n2
          4 -> Bxc
          5 -> Out op
          6 -> Bdv op
          7 -> Cdv op
          _ -> undefined
        op = case n2 of
          4 -> Ptr A
          5 -> Ptr B
          6 -> Ptr C
          v -> Lit v
    parseInp (_ : []) = undefined
    parseInp [] = []

dfsQuine :: [Instr] -> [Int] -> Maybe Int
dfsQuine prog quine = inner (reverse quine) 0
  where
    inner :: [Int] -> Int -> Maybe Int
    inner (q : qs) basis =
      listToMaybe $ catMaybes $ map (inner qs) $ filter ((==) q . head . runProg prog) $ map ((+) nbasis) $ [0 .. 8]
      where
        nbasis :: Int = basis `shiftL` 3
    inner [] basis = Just basis

data RegStates = RegStates {a :: Int, b :: Int, c :: Int, out :: [Int]}

runProg :: [Instr] -> Int -> [Int]
runProg prog a0 = reverse $ out $ inner (RegStates {a = a0, b = 0, c = 0, out = []}) 0
  where
    inner :: RegStates -> Int -> RegStates
    inner regs isp = case prog !? isp of
      Just instr -> case instr of
        Adv op -> inner regs {a = (a regs) `shiftR` (parseOp op)} (isp + 1)
        Bxl lt -> inner regs {b = (b regs) `xor` lt} (isp + 1)
        Bst op -> inner regs {b = parseOp op `rem` 8} (isp + 1)
        Jnz lt -> inner regs (if a regs /= 0 then lt else (isp + 1))
        Bxc    -> inner regs {b = (b regs) `xor` (c regs)} (isp + 1)
        Out op -> inner regs {out = (parseOp op `rem` 8) : (out regs)} (isp + 1)
        Bdv op -> inner regs {b = (a regs) `shiftR` (parseOp op)} (isp + 1)
        Cdv op -> inner regs {c = (a regs) `shiftR` (parseOp op)} (isp + 1)
      Nothing -> regs
      where
        parseOp :: ComboOp -> Int
        parseOp (Lit n) = n
        parseOp (Ptr A) = a regs
        parseOp (Ptr B) = b regs
        parseOp (Ptr C) = c regs
