module AoC2024.Solutions.Day2 (parse, part1, part2) where

import AoC2024.Utils (differences, (&&&), count, faces)

safe :: [Int] -> Bool
safe report = 
  let ds = differences report in 
  all ((>= -3) &&& (<= -1)) ds || all ((>= 1) &&& (<= 3)) ds

parse :: String -> [[Int]]
parse = map (map read . words) . lines

part1 :: [[Int]] -> Int
part1 = count safe

part2 :: [[Int]] -> Int
part2 = count (any safe . faces)