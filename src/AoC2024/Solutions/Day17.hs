{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module AoC2024.Solutions.Day17 (parse, part1, part2) where

import Data.Functor ((<&>))
import Data.Bits
import Data.Array (Array)
import qualified Data.Array as Array
import AoC2024.Parser
import AoC2024.Utils

type Registers = (Int, Int, Int)
type ProgState = (Registers, Int, [Int])

step :: Array Int Int -> ProgState -> Maybe ProgState
step instrs (regs@(regA, regB, regC), i, out) =
  instrs !? i <&> \case 
    0 -> ((regA `shiftR` comboOp, regB, regC), i + 2, out)
    1 -> ((regA, regB .^. litOp, regC), i + 2, out)
    2 -> ((regA, comboOp .&. 7, regC), i + 2, out)
    3 -> (regs, if regA == 0 then i + 2 else litOp, out)
    4 -> ((regA, regB .^. regC, regC), i + 2, out)
    5 -> (regs, i + 2, (comboOp .&. 7) : out)
    6 -> ((regA, regA `shiftR` comboOp, regC), i + 2, out)
    7 -> ((regA, regB, regA `shiftR` comboOp), i + 2, out)
    _ -> (regs, i + 2, out)
  where
    litOp = instrs ! (i + 1)
    comboOp = case litOp of
      4 -> regA
      5 -> regB
      6 -> regC
      _ -> litOp

runProgram :: Array Int Int -> Registers -> [Int] 
runProgram instrs regs = out
  where (_, _, out) = whileJust (step instrs) (regs, 0, [])

parse :: String -> (Registers, Array Int Int)
parse = partialExecParser p 
  where
    register :: Char -> Parser Int
    register c = id <$ string "Register " <* char c <* string ": " <*> natural <* char '\n'

    registers :: Parser Registers
    registers = (,,) <$> register 'A' <*> register 'B' <*> register 'C'

    program :: Parser (Array Int Int)
    program = do
      list <- id <$ string "Program: " <*> separatedBy (char ',') natural <* char '\n'
      return (Array.listArray (0, length list - 1) list)
    
    p = (,) <$> registers <* char '\n' <*> program <* eof

part1 :: (Registers, Array Int Int) -> String
part1 (regs, instrs) =
  let out = runProgram instrs regs in
  foldl (\str n -> if null str then show n else show n ++ ',' : str) "" out -- TODO: add a "join" function to Utils?

part2 :: (Registers, Array Int Int) -> Int 
part2 ((_, regB, regC), instrs) = let
  regs = map (, regB, regC) [1..]
  outs = map (runProgram instrs) regs
  instrList = reverse (Array.elems instrs)
  in 1 + indexWhere (== instrList) outs