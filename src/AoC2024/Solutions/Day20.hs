{-# LANGUAGE TupleSections #-}
module AoC2024.Solutions.Day20 (parse, part1) where

import AoC2024.Utils

getNeighbors :: Array2 Bool -> Maybe (Int, Int) -> (Int, Int) -> [((Int, Int), Int)]
getNeighbors maze removedWall = 
  map (, 1) . filter (\i -> maze ! i || removedWall == Just i) . neighbors maze

parse :: String -> (Array2 Bool, (Int, Int), (Int, Int))
parse str = let
  grid = fromList (lines str)
  start = indexOf 'S' grid
  end = indexOf 'E' grid
  in (fmap (/= '#') grid, start, end)

part1 :: (Array2 Bool, (Int, Int), (Int, Int)) -> Int
part1 (maze, start, end) = let
  dist = distances (getNeighbors maze Nothing) start ! end
  walls = indicesOf False maze
  dists = map (\wall -> distances (getNeighbors maze (Just wall)) start ! end) walls
  in count (<= dist - 100) dists