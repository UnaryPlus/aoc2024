module AoC2024.Solutions.Day10 (parse, part1, part2) where

import Data.Foldable (fold)
import Data.Char (digitToInt)
import Data.Monoid (Sum, getSum)
import Data.Set (Set)
import qualified Data.Set as Set
import AoC2024.Utils (sumMap, Grid, neighborElems, fromList, mapWithIndex, neighborElems)

peaks :: Grid Int -> Grid (Set (Int, Int), Sum Int)
peaks = 
  mapWithIndex (\i x -> 
    if x == 9 then (Set.singleton i, 1) 
    else (Set.empty, 0)
  )

getReachable :: Grid Int -> Int -> Grid (Set (Int, Int), Sum Int) -> Grid (Set (Int, Int), Sum Int)
getReachable grid height prev =
  mapWithIndex (\i x -> 
    if x == height then fold (neighborElems prev i)
    else (Set.empty, 0)
  ) grid

parse :: String -> Grid Int 
parse = fromList . map (map digitToInt) . lines

part1 :: Grid Int -> Int
part1 grid = let
  reachable = foldr (getReachable grid) (peaks grid) [0..8]
  in sumMap (length . fst) reachable

part2 :: Grid Int -> Int
part2 grid = let
  reachable = foldr (getReachable grid) (peaks grid) [0..8]
  in getSum (foldMap snd reachable)