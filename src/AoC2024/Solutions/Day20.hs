{-# LANGUAGE TupleSections #-}
module AoC2024.Solutions.Day20 (parse, part1, part2, walkPath) where

import qualified Data.Array as Array
import AoC2024.Utils

walkPath :: Array2 Bool -> (Int, Int) -> [(Int, Int)]
walkPath = walkPath' Nothing
  where
    walkPath' prev maze i =
      case filter (\j -> maze ! j && prev /= Just j) (neighbors maze i) of
        [] -> [i]
        next:_ -> i : walkPath' (Just i) maze next

pathPositions :: Array2 Bool -> (Int, Int) -> Array2 Int
pathPositions maze start = Array.array (Array.bounds maze) (zip (walkPath maze start) [0..])

cheats :: Array2 Bool -> Int -> [((Int, Int), (Int, Int), Int)]
cheats maze dist = do 
  i <- indicesOf True maze
  r <- [2..dist]
  map (i,,r) (filter (maze !) (manhattanCircle maze r i))

parse :: String -> (Array2 Bool, (Int, Int))
parse str = let
  grid = fromList (lines str)
  start = indexOf 'S' grid
  in (fmap (/= '#') grid, start)

part1 :: (Array2 Bool, (Int, Int)) -> Int
part1 (maze, start) = 
  let positions = pathPositions maze start in
  count (\(i, j, r) -> positions ! j - positions ! i - r >= 100) (cheats maze 2)

part2 :: (Array2 Bool, (Int, Int)) -> Int
part2 (maze, start) = 
  let positions = pathPositions maze start in
  count (\(i, j, r) -> positions ! j - positions ! i - r >= 100) (cheats maze 20)
  