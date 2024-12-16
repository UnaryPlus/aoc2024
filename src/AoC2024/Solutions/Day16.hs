module AoC2024.Solutions.Day16 (parse, part1, part2) where

import qualified Data.Set as Set
import AoC2024.Utils (add2, minimumOn, Grid, fromList, (!), indexOf, distances, generalDijkstra)

type Direction = (Int, Int)

up, down, left, right :: Direction
up = (-1, 0)
down = (1, 0)
left = (0, -1)
right = (0, 1)

cardinalDirections :: [Direction]
cardinalDirections = [ up, down, left, right ]

turnLeft, turnRight :: Direction -> Direction
turnLeft (x, y) = (-y, x)
turnRight (x, y) = (y, -x)

type Node = ((Int, Int), Direction)

getNeighbors :: Grid Bool -> Node -> [(Node, Int)]
getNeighbors maze (pos, dir) = let
  rotated = [ ((pos, turnLeft dir), 1000), ((pos, turnRight dir), 1000) ]
  pos' = add2 pos dir
  in if maze ! pos' then ((pos', dir), 1) : rotated else rotated

parse :: String -> (Grid Bool, (Int, Int), (Int, Int))
parse str = let
  grid = fromList (lines str)
  start = indexOf 'S' grid
  end = indexOf 'E' grid
  in (fmap (/= '#') grid, start, end)

part1 :: (Grid Bool, (Int, Int), (Int, Int)) -> Int
part1 (maze, start, end) = let
  dists = distances (getNeighbors maze) (start, right)
  in minimum (map (\dir -> dists ! (end, dir)) cardinalDirections)

part2 :: (Grid Bool, (Int, Int), (Int, Int)) -> Int
part2 (maze, start, end) = let
  dists = generalDijkstra (Set.singleton . fst) (\nbr _ -> Set.insert (fst nbr)) (<>) (getNeighbors maze) (start, right)
  bestDir = minimumOn (\dir -> fst (dists ! (end, dir))) cardinalDirections
  in length (snd (dists ! (end, bestDir)))