{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module AoC2024.Solutions.Day21 where -- (parse, part1, part2) where

import Data.Map (Map)
import qualified Data.Map as Map
import AoC2024.Utils (nest, sumMap, mapPairs)

data Key = U | D | L | R | A
  deriving (Eq, Ord, Show)

numPosition :: Char -> (Int, Int)
numPosition = \case
  '7' -> (0, 0)
  '8' -> (0, 1)
  '9' -> (0, 2)
  '4' -> (1, 0)
  '5' -> (1, 1)
  '6' -> (1, 2)
  '1' -> (2, 0)
  '2' -> (2, 1)
  '3' -> (2, 2)
  '0' -> (3, 1)
  'A' -> (3, 2)
  _ -> undefined

dirPosition :: Key -> (Int, Int)
dirPosition = \case
  U -> (0, 1)
  A -> (0, 2)
  L -> (1, 0)
  D -> (1, 1)
  R -> (1, 2)

verticalSpan :: Int -> Int -> [Key]
verticalSpan r1 r2 = 
  replicate (abs diff) (if diff < 0 then U else D)
  where diff = r2 - r1

horizontalSpan :: Int -> Int -> [Key]
horizontalSpan c1 c2 =
  replicate (abs diff) (if diff < 0 then L else R)
  where diff = c2 - c1

-- (left, up) is better than (up, left) when possible
-- (left, down) is better than (down, left) when possible
-- (down, right) is better than (right, down) when possible
-- (up, right) is better than (right, up) when possible

preferredSpan :: (Int, Int) -> (Int, Int) -> ([Key], [Key])
preferredSpan (r1, c1) (r2, c2)
  | c2 < c1 = (horizontalSpan c1 c2, verticalSpan r1 r2)
  | otherwise = (verticalSpan r1 r2, horizontalSpan c1 c2)

numSpan :: (Int, Int) -> (Int, Int) -> [Key]
numSpan v1@(r1, c1) v2@(r2, c2)
  | (c1 == 0 && r2 == 3) || (r1 == 3 && c2 == 0) = keys2 ++ keys1 ++ [A]
  | otherwise = keys1 ++ keys2 ++ [A]
  where (keys1, keys2) = preferredSpan v1 v2

dirSpan :: (Int, Int) -> (Int, Int) -> [Key]
dirSpan v1@(r1, c1) v2@(r2, c2)
  | (c1 == 0 && r2 == 0) || (r1 == 0 && c2 == 0) = keys2 ++ keys1 ++ [A]
  | otherwise = keys1 ++ keys2 ++ [A]
  where (keys1, keys2) = preferredSpan v1 v2

liftNum :: [Char] -> [Key]
liftNum = concat . mapPairs numSpan . map numPosition . ('A':)

liftDir :: [Key] -> [Key]
liftDir = concat . mapPairs dirSpan . map dirPosition . (A:)

chunks :: [Key] -> [[Key]]
chunks keys
  | null keys = []
  | otherwise = 
    case span (/= A) keys of
      (keys1, A:keys2) -> (keys1 ++ [A]) : chunks keys2 
      _ -> undefined

chunkMap :: [Key] -> Map [Key] Int
chunkMap = Map.fromListWith (+) . map (, 1) . chunks 

liftDirOptimized :: Map [Key] Int -> Map [Key] Int
liftDirOptimized = 
  Map.fromListWith (+) . concatMap lift . Map.toList
  where lift (chunk, n) = map (, n) (chunks (liftDir chunk))

totalLength :: Map [Key] Int -> Int
totalLength = sumMap (\(chunk, n) -> length chunk * n) . Map.toList

parse :: String -> [(Int, [Char])]
parse = map (\cs -> (read (init cs), cs)) . lines

part1 :: [(Int, [Char])] -> Int
part1 = sumMap complexity
  where 
    complexity (n, cs) = 
      n * length ((liftDir . liftDir . liftNum) cs)

part2 :: [(Int, [Char])] -> Int
part2 = sumMap complexity
  where 
    complexity (n, cs) = 
      n * totalLength ((nest 25 liftDirOptimized . chunkMap . liftNum) cs)