module AoC2024.Solutions.Day8 (parse, part1, part2) where

import Data.Char (isAlphaNum)
import Data.List (nub)
import Data.Ix (range, inRange)
import qualified Data.Map as Map
import AoC2024.Utils (count)

antinode :: (Int, Int) -> (Int, Int) -> (Int, Int)
antinode (x1, x2) (y1, y2) = (2 * y1 - x1, 2 * y2 - x2)

antinodes :: [(Int, Int)] -> [(Int, Int)]
antinodes pts = [ antinode x y | x <- pts, y <- pts, x /= y ]

antinode2 :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
antinode2 (m, n) (x1, x2) (y1, y2) =
  takeWhile (inRange ((0, 0), (m - 1, n - 1)))
  [ ((i + 1) * y1 - i * x1, (i + 1) * y2 - i * x2) | i <- [0..]] -- Infinite list!

antinodes2 :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
antinodes2 dims pts = concat [ antinode2 dims x y | x <- pts, y <- pts, x /= y ]

parse :: String -> ((Int, Int), [[(Int, Int)]])
parse str = let
  ls = lines str
  (m, n) = (length ls, length (head ls))
  pairs = zip (concat ls) (range ((0, 0), (m - 1, n - 1))) 
  ixMap = Map.fromListWith (++) [ (c, [ix]) | (c, ix) <- pairs, isAlphaNum c ]
  in ((m, n), Map.elems ixMap)

part1 :: ((Int, Int), [[(Int, Int)]]) -> Int
part1 ((m, n), pts) = count (inRange ((0, 0), (m - 1, n - 1))) (nub (concatMap antinodes pts))

part2 :: ((Int, Int), [[(Int, Int)]]) -> Int
part2 (dims, pts) = length (nub (concatMap (antinodes2 dims) pts))