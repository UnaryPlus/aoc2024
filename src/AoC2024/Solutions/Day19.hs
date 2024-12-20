module AoC2024.Solutions.Day19 (parse, part1, part2) where

import Data.List
import qualified Data.Array as Array
import AoC2024.Parser 
import AoC2024.Utils

numDivisionsInto :: Eq a => [[a]] -> [a] -> Int
numDivisionsInto towels design = arr ! length design
  where
    subdesigns = tail (reverse (tails design))
    arr = Array.listArray (0, length design) (1 : zipWith genNext [1..] subdesigns)
    genNext i subdesign = sumMap (\towel ->
      if towel `isPrefixOf` subdesign then arr ! (i - length towel) else 0
      ) towels

parse :: String -> ([String], [String])
parse = partialExecParser p
  where
    p = (,) <$> towels <* string "\n\n" <*> designs <* eof

    towels = separatedBy (string ", ") (parseWhile isColor)
    designs = endedBy (char '\n') (parseWhile isColor)

    isColor c = c `elem` "rgubw"

part1 :: ([String], [String]) -> Int
part1 (towels, designs) = count ((> 0) . numDivisionsInto towels) designs

part2 :: ([String], [String]) -> Int
part2 (towels, designs) = sumMap (numDivisionsInto towels) designs 