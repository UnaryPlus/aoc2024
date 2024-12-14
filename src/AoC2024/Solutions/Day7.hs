{-# LANGUAGE LambdaCase #-}
module AoC2024.Solutions.Day7 (parse, part1, part2) where

import Data.List (foldl')
import AoC2024.Utils (sumMap)
import AoC2024.Parser (char, partialExecParser, natural, separatedBy, string) 

concatDigits :: Int -> Int -> Int
concatDigits i j = i * 10 ^ length (show j) + j

results :: [Int -> Int -> Int] -> [Int] -> [Int]
results ops = \case
  [] -> []
  x:xs -> foldl' (\rs y -> [op r y | op <- ops, r <- rs]) [x] xs

parse :: String -> [(Int, [Int])]
parse = map (partialExecParser p) . lines
  where
    p = (,) <$> natural <* string ": " <*> separatedBy (char ' ') natural

part1 :: [(Int, [Int])] -> Int
part1 = sumMap fst . filter (\(result, xs) -> result `elem` results [(+), (*)] xs)

part2 :: [(Int, [Int])] -> Int
part2 = sumMap fst . filter (\(result, xs) -> result `elem` results [(+), (*), concatDigits] xs) 