{-# LANGUAGE LambdaCase #-}

module AoC2024.Solutions.Day7 (parse, part1, part2) where

import Data.List (foldl')
import Data.Maybe (fromMaybe)
import AoC2024.Parser (char, execParser, natural, separatedBy, string) 

concatDigits :: Int -> Int -> Int
concatDigits i j = i * 10 ^ length (show j) + j

results :: [Int -> Int -> Int] -> [Int] -> [Int]
results ops = \case
  [] -> []
  x:xs -> foldl' (\rs y -> [op r y | op <- ops, r <- rs]) [x] xs

parse :: String -> [(Int, [Int])]
parse = map (fromMaybe (error "Parsing failed") . execParser p) . lines
  where
    p = (,) <$> natural <* string ": " <*> separatedBy (char ' ') natural

part1 :: [(Int, [Int])] -> Int
part1 = sum . map fst . filter (\(result, xs) -> result `elem` results [(+), (*)] xs)

part2 :: [(Int, [Int])] -> Int
part2 = sum . map fst . filter (\(result, xs) -> result `elem` results [(+), (*), concatDigits] xs) 