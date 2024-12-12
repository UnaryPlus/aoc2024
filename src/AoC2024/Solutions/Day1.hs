module AoC2024.Solutions.Day1 (parse, part1, part2) where

import Data.List (sort)
import AoC2024.Utils (sumMap, alternate, count)

parse :: String -> ([Int], [Int])
parse = alternate . map read . words

part1 :: ([Int], [Int]) -> Int
part1 (xs, ys) =
  sumMap abs (zipWith (-) (sort xs) (sort ys))

-- Inefficient but runs fast enough
part2 :: ([Int], [Int]) -> Int
part2 (xs, ys) =
  sumMap (\x -> x * count (== x) ys) xs