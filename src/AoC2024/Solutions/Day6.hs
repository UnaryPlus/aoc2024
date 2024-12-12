{-# LANGUAGE LambdaCase #-}

module AoC2024.Solutions.Day6 (parse, part1, part2) where

import Control.Monad.ST (ST, runST)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array.ST (STArray, readArray, writeArray, newArray, freeze)
import Data.Array (range, (!))
import qualified Data.Array as Array
import AoC2024.Utils (add2, Grid, fromList, indexOf, (!?), count, modifiedCopy)

type Direction = (Int, Int)

up :: Direction
up = (-1, 0)

turnRight :: Direction -> Direction
turnRight (x, y) = (y, -x)

guardLoop :: Grid Bool -> (Int, Int) -> Direction -> STArray s (Int, Int) (Set Direction) -> ST s Bool
guardLoop grid pos dir history = do
  prevDirs <- readArray history pos
  if dir `elem` prevDirs then return True
  else do
    writeArray history pos (Set.insert dir prevDirs)
    let pos' = add2 pos dir
    case grid !? pos' of
      Nothing -> return False
      Just False -> guardLoop grid pos' dir history
      Just True -> guardLoop grid pos (turnRight dir) history
  
squaresVisited :: Grid Bool -> (Int, Int) -> Grid Bool
squaresVisited grid start = runST $ do
  history <- newArray (Array.bounds grid) Set.empty
  _ <- guardLoop grid start up history
  history' <- freeze history
  return (fmap (not . Set.null) history')

circlesForever :: Grid Bool -> (Int, Int) -> Bool
circlesForever grid start = runST $ do
  history <- newArray (Array.bounds grid) Set.empty
  guardLoop grid start up history

parse :: String -> (Grid Bool, (Int, Int))
parse str = let
  chars = fromList (lines str)
  bools = fmap (== '#') chars
  in (bools, indexOf '^' chars)

part1 :: (Grid Bool, (Int, Int)) -> Int
part1 (grid, start) = count id (squaresVisited grid start)

-- Takes very long to run in GHCi, but not in test suite (???)
part2 :: (Grid Bool, (Int, Int)) -> Int
part2 (grid, start) = let 
  visited = squaresVisited grid start
  toCheck = filter (\i -> visited ! i && i /= start) (range (Array.bounds grid))
  in count (\i -> circlesForever (modifiedCopy i True grid) start) toCheck