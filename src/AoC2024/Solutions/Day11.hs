{-# LANGUAGE TupleSections #-}
module AoC2024.Solutions.Day11 (parse, part1, part2) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import AoC2024.Utils (applyN)

updateStone :: Int -> [Int]
updateStone x
  | x == 0 = [1]
  | m == 0 = [fst halves, snd halves]
  | otherwise = [2024 * x]
  where
    (d, m) = length (show x) `divMod` 2
    halves = x `divMod` (10 ^ d)

blink :: IntMap Int -> IntMap Int
blink = IntMap.fromListWith (+) . IntMap.foldrWithKey push []
  where 
    push x mul xs = map (, mul) (updateStone x) ++ xs

parse :: String -> IntMap Int
parse = IntMap.fromListWith (+) . map ((, 1) . read) . words

part1 :: IntMap Int -> Int
part1 = sum . applyN 25 blink

part2 :: IntMap Int -> Int
part2 = sum . applyN 75 blink