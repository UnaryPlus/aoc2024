module AoC2024.Solutions.Day16 (parse, part1, part2) where

import qualified Data.Set as Set
import AoC2024.Utils (add2, right, cardinalDirections, turnLeft, turnRight, minimumOn, Array2, fromList, (!), indexOf, graphDistances, generalDijkstra)

type Node = ((Int, Int), (Int, Int))

getNeighbors :: Array2 Bool -> Node -> [(Node, Int)]
getNeighbors maze (pos, dir) = let
  rotated = [ ((pos, turnLeft dir), 1000), ((pos, turnRight dir), 1000) ]
  pos' = add2 pos dir
  in if maze ! pos' then ((pos', dir), 1) : rotated else rotated

parse :: String -> (Array2 Bool, (Int, Int), (Int, Int))
parse str = let
  grid = fromList (lines str)
  start = indexOf 'S' grid
  end = indexOf 'E' grid
  in (fmap (/= '#') grid, start, end)

part1 :: (Array2 Bool, (Int, Int), (Int, Int)) -> Int
part1 (maze, start, end) = let
  dists = graphDistances (getNeighbors maze) (start, right)
  in minimum (map (\dir -> dists ! (end, dir)) cardinalDirections)

part2 :: (Array2 Bool, (Int, Int), (Int, Int)) -> Int
part2 (maze, start, end) = let
  dists = generalDijkstra (Set.singleton . fst) (\nbr _ -> Set.insert (fst nbr)) (<>) (getNeighbors maze) (start, right)
  bestDir = minimumOn (\dir -> fst (dists ! (end, dir))) cardinalDirections
  in length (snd (dists ! (end, bestDir)))