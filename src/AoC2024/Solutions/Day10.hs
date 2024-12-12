module AoC2024.Solutions.Day10 (parse, part1, part2) where

import Data.Char (digitToInt)
import Data.Monoid (Sum, getSum)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array ((!), inRange)
import qualified Data.Array as Array
import AoC2024.Utils (Grid, fromList, mapWithIndex)

peaks :: Grid Int -> Grid (Set (Int, Int), Sum Int)
peaks = mapWithIndex (\i x -> if x == 9 then (Set.singleton i, 1) else (Set.empty, 0))

getReachable :: Grid Int -> Int -> Grid (Set (Int, Int), Sum Int) -> Grid (Set (Int, Int), Sum Int)
getReachable grid height prev =
  mapWithIndex update grid
  where
    bounds = Array.bounds grid
    update (r, c) x 
      | x == height = let
        neighbors = filter (inRange bounds) [ (r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1) ]
        in foldMap (prev !) neighbors
      | otherwise = (Set.empty, 0)

parse :: String -> Grid Int 
parse = fromList . map (map digitToInt) . lines

part1 :: Grid Int -> Int
part1 grid = let
  reachable = foldr (getReachable grid) (peaks grid) [0..8]
  in sum (fmap (length . fst) reachable)

part2 :: Grid Int -> Int
part2 grid = let
  reachable = foldr (getReachable grid) (peaks grid) [0..8]
  in getSum (foldMap snd reachable)