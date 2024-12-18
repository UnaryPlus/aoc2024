{-# LANGUAGE LambdaCase #-}
module AoC2024.Solutions.Day17 (parse, part1, part2) where

import Data.Functor ((<&>))
import Data.Bits ((.^.), (.&.), shiftR)
import Data.Array (Array)
import qualified Data.Array as Array
import AoC2024.Parser (Parser, partialExecParser, char, string, natural, eof, separatedBy)
import AoC2024.Utils ((!?), (!), whileJust)

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
  foldl (\str n -> if null str then show n else show n ++ ',' : str) "" out

-- Although part1 (along with parse) is capable of interpreting any program in this assembly language,
-- part2 is specific to the program in input/day17.txt. This program outputs [f x, f (x >> 3), f (x >> 6), ...],
-- where f is defined as below and x is the initial value of register A. (The number of outputs is the number of
-- digits in the octal representation of x.) Using this knowledge, we can work backwards, considering first the
-- numbers that output [0], then those that output [3, 0], then [5, 3, 0], and so on. If we call these sets of
-- numbers X1, X2, X3, ..., then every number in X{n+1} must be of the form 8 * m + k, where m is in X{n} and 
-- 0 <= k <= 7. The answer to part 2 is the minimum number in X16.
part2 :: (a, Array Int Int) -> Int
part2 (_, instrs) = minimum possibilities
  where
    f x = (((x .&. 7) .^. 3) .^. (x `shiftR` ((x .&. 7) .^. 5))) .&. 7
    extendToInclude n = concatMap (\x -> filter ((== n) . f) [x*8 .. x*8+7])
    possibilities = foldr extendToInclude [0] (Array.elems instrs)