module AoC2024.Solutions.Day6 (parse, part1, part2) where

import Control.Monad.ST (ST, runST)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ix (range)
import Data.Array.ST (readArray, writeArray, newArray)
import qualified Data.Array as Array
import AoC2024.Utils (add2, up, turnRight, Array2, STArray2, fromList, indexOf, (!), (!?), count, modifiedCopy, freeze)

type Direction = (Int, Int)

guardLoop :: Array2 Bool -> (Int, Int) -> Direction -> STArray2 s (Set Direction) -> ST s Bool
guardLoop grid pos dir history = do
  prevDirs <- readArray history pos
  if Set.member dir prevDirs then return True
  else do
    writeArray history pos (Set.insert dir prevDirs)
    let pos' = add2 pos dir
    case grid !? pos' of
      Nothing -> return False
      Just False -> guardLoop grid pos' dir history
      Just True -> guardLoop grid pos (turnRight dir) history
  
squaresVisited :: Array2 Bool -> (Int, Int) -> Array2 Bool
squaresVisited grid start = runST $ do
  history <- newArray (Array.bounds grid) Set.empty
  _ <- guardLoop grid start up history
  history' <- freeze history
  return (fmap (not . Set.null) history')

circlesForever :: Array2 Bool -> (Int, Int) -> Bool
circlesForever grid start = runST $ do
  history <- newArray (Array.bounds grid) Set.empty
  guardLoop grid start up history

parse :: String -> (Array2 Bool, (Int, Int))
parse str = let
  chars = fromList (lines str)
  bools = fmap (== '#') chars
  in (bools, indexOf '^' chars)

part1 :: (Array2 Bool, (Int, Int)) -> Int
part1 (grid, start) = count id (squaresVisited grid start)

part2 :: (Array2 Bool, (Int, Int)) -> Int
part2 (grid, start) = let 
  visited = squaresVisited grid start
  toCheck = filter (\i -> visited ! i && i /= start) (range (Array.bounds grid))
  in count (\i -> circlesForever (modifiedCopy i True grid) start) toCheck