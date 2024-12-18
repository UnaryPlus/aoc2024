{-# LANGUAGE TupleSections #-}
module AoC2024.Solutions.Day18 (parse, part1, part2) where

import qualified Data.Array as Array
import AoC2024.Parser (partialExecParser, natural, char)
import AoC2024.Utils ((!), Array2, neighbors, distances)

getNeighbors :: Array2 Bool -> (Int, Int) -> [((Int, Int), Int)]
getNeighbors maze = map (, 1) . filter (maze !) . neighbors maze

parse :: String -> [(Int, Int)]
parse = map (partialExecParser p) . lines
  where p = (,) <$> natural <* char ',' <*> natural

part1 :: [(Int, Int)] -> Int
part1 bytes = let
  kilo = take 1024 bytes 
  maze = Array.accumArray (&&) True ((0, 0), (70, 70)) (map (, False) kilo)
  in distances (getNeighbors maze) (0, 0) ! (70, 70)

part2 :: [(Int, Int)] -> Int
part2 = undefined