module Main (main) where

import System.Exit (exitFailure)
import Control.Monad (when)

import AoC2024.Interpolate (interpolate)
import qualified AoC2024.Solutions.Day1 as Day1
import qualified AoC2024.Solutions.Day2 as Day2
import qualified AoC2024.Solutions.Day3 as Day3
import qualified AoC2024.Solutions.Day4 as Day4
import qualified AoC2024.Solutions.Day5 as Day5
import qualified AoC2024.Solutions.Day6 as Day6
import qualified AoC2024.Solutions.Day7 as Day7
import qualified AoC2024.Solutions.Day8 as Day8
import qualified AoC2024.Solutions.Day9 as Day9
import qualified AoC2024.Solutions.Day10 as Day10
import qualified AoC2024.Solutions.Day11 as Day11
import qualified AoC2024.Solutions.Day12 as Day12
import qualified AoC2024.Solutions.Day13 as Day13
import qualified AoC2024.Solutions.Day14 as Day14
import qualified AoC2024.Solutions.Day15 as Day15
import qualified AoC2024.Solutions.Day16 as Day16
import qualified AoC2024.Solutions.Day17 as Day17
import qualified AoC2024.Solutions.Day18 as Day18
import qualified AoC2024.Solutions.Day19 as Day19
import qualified AoC2024.Solutions.Day20 as Day20
import qualified AoC2024.Solutions.Day21 as Day21
import qualified AoC2024.Solutions.Day22 as Day22
import qualified AoC2024.Solutions.Day23 as Day23
import qualified AoC2024.Solutions.Day24 as Day24
import qualified AoC2024.Solutions.Day25 as Day25

testDay :: (Eq b, Eq c, Show b, Show c) => Int -> (String -> a) -> (a -> b) -> (a -> c) -> FilePath -> b -> c -> IO ()
testDay n parse part1 part2 path answer1 answer2 = do
  interpolate "Testing day $..." n
  input <- parse <$> readFile path
  let result1 = part1 input
  when (result1 /= answer1) $ do
    interpolate "Day $, part 1: expected $, got $" n answer1 result1
    exitFailure 
  let result2 = part2 input
  when (result2 /= answer2) $ do
    interpolate "Day $, part 2: expected $, got $" n answer2 result2
    exitFailure

main :: IO ()
main = do
  testDay 1 Day1.parse Day1.part1 Day1.part2 "input/day1.txt" 2285373 21142653
  testDay 2 Day2.parse Day2.part1 Day2.part2 "input/day2.txt" 686 717
  testDay 3 Day3.parse Day3.part1 Day3.part2 "input/day3.txt" 174336360 88802350
  testDay 4 Day4.parse Day4.part1 Day4.part2 "input/day4.txt" 2378 1796
  testDay 5 Day5.parse Day5.part1 Day5.part2 "input/day5.txt" 5129 4077
  testDay 6 Day6.parse Day6.part1 Day6.part2 "input/day6.txt" 4656 1575
  testDay 7 Day7.parse Day7.part1 Day7.part2 "input/day7.txt" 3119088655389 264184041398847
  testDay 8 Day8.parse Day8.part1 Day8.part2 "input/day8.txt" 269 949
  testDay 9 Day9.parse Day9.part1 Day9.part2 "input/day9.txt" 6382875730645 6420913943576
  testDay 10 Day10.parse Day10.part1 Day10.part2 "input/day10.txt" 760 1764
  testDay 11 Day11.parse Day11.part1 Day11.part2 "input/day11.txt" 189547 224577979481346
  testDay 12 Day12.parse Day12.part1 Day12.part2 "input/day12.txt" 1446042 902742
  testDay 13 Day13.parse Day13.part1 Day13.part2 "input/day13.txt" 36758 76358113886726
  testDay 14 Day14.parse Day14.part1 Day14.part2 "input/day14.txt" 230686500 7672
  testDay 15 Day15.parse Day15.part1 Day15.part2 "input/day15.txt" 1360570 1381446
  testDay 16 Day16.parse Day16.part1 Day16.part2 "input/day16.txt" 143564 593
  testDay 17 Day17.parse Day17.part1 Day17.part2 "input/day17.txt" "3,6,3,7,0,7,0,3,0" 136904920099226
  testDay 18 Day18.parse Day18.part1 Day18.part2 "input/day18.txt" 338 "20,44" -- Rather slow
  testDay 19 Day19.parse Day19.part1 Day19.part2 "input/day19.txt" 336 758890600222015
  testDay 20 Day20.parse Day20.part1 Day20.part2 "input/day20.txt" 1384 1008542      
  testDay 21 Day21.parse Day21.part1 Day21.part2 "input/day21.txt" 203734 246810588779586
  testDay 22 Day22.parse Day22.part1 Day22.part2 "input/day22.txt" 12759339434 1405
  testDay 23 Day23.parse Day23.part1 Day23.part2 "input/day23.txt" 1184 "hf,hz,lb,lm,ls,my,ps,qu,ra,uc,vi,xz,yv"
  testDay 24 Day24.parse Day24.part1 Day24.part2 "input/day24.txt" 64755511006320 "djg,dsd,hjm,mcq,sbg,z12,z19,z37"
  testDay 25 Day25.parse Day25.part1 (const ()) "input/day25.txt" 3136 ()
