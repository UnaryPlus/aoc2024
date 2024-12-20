{-# LANGUAGE TupleSections #-}
module AoC2024.Solutions.Day18 (parse, part1, part2) where

import Data.Maybe (fromJust, isJust)
import AoC2024.Parser (partialExecParser, natural, char)
import AoC2024.Utils (Array2, (!), falseArray, modifiedCopy, neighbors, graphDistance)

getNeighbors :: Array2 Bool -> (Int, Int) -> [((Int, Int), Int)]
getNeighbors maze = map (, 1) . filter (maze !) . neighbors maze

parse :: String -> [(Int, Int)]
parse = map (partialExecParser p) . lines
  where p = (,) <$> natural <* char ',' <*> natural

part1 :: [(Int, Int)] -> Int
part1 bytes = let
  kilo = take 1024 bytes 
  maze = falseArray ((0, 0), (70, 70)) kilo
  in fromJust (graphDistance (getNeighbors maze) (0, 0) (70, 70))

-- Rather slow
part2 :: [(Int, Int)] -> String
part2 bytes = let
  (kilo, rest) = splitAt 1024 bytes
  start = falseArray ((0, 0), (70, 70)) kilo
  mazes = tail (scanl (\maze byte -> modifiedCopy byte False maze) start rest)
  possible = map (\maze -> isJust (graphDistance (getNeighbors maze) (0, 0) (70, 70))) mazes
  badByte = fst (head (dropWhile snd (zip rest possible)))
  in show (fst badByte) ++ "," ++ show (snd badByte)