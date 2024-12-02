{-# LANGUAGE LambdaCase #-}

import Data.List (sort, foldl')
import Data.Map (Map)

main :: IO ()
main = print =<< day2Part2 <$> parse2

alternate :: [a] -> ([a], [a])
alternate = \case
  [] -> ([], [])
  [x] -> ([x], [])
  x:y:zs -> let (xs, ys) = alternate zs in (x:xs, y:ys)

count :: (a -> Bool) -> [a] -> Int
count p = foldl' (\n x -> if p x then n + 1 else n) 0 

multiplicity :: Eq a => a -> [a] -> Int
multiplicity a = count (== a)

differences :: Num a => [a] -> [a]
differences = \case
  [] -> []
  [_] -> []
  x:xs@(y:_) -> (y - x) : differences xs

infixr 3 &&&
(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) p q x = p x && q x

infixr 2 |||
(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) p q x = p x || q x

safe :: [Int] -> Bool
safe report = let
  ds = differences report
  in all ((>= -3) &&& (<= -1)) ds || all ((>= 1) &&& (<= 3)) ds

-- Name comes from thinking of the list as a simplex
faces :: [a] -> [[a]]
faces [] = []
faces (x:xs) = xs : map (x:) (faces xs)

parse1 :: IO ([Int], [Int])
parse1 = alternate . map read . words <$> readFile "input/day1.txt"

day1Part1 :: ([Int], [Int]) -> Int
day1Part1 (xs, ys) =
  sum (map abs (zipWith (-) (sort xs) (sort ys)))

-- Inefficient but runs fast enough
day1Part2 :: ([Int], [Int]) -> Int
day1Part2 (xs, ys) =
  sum (map (\x -> x * count (== x) ys) xs)

parse2 :: IO [[Int]]
parse2 = map (map read . words) . lines <$> readFile "input/day2.txt"

day2Part1 :: [[Int]] -> Int
day2Part1 = count safe

day2Part2 :: [[Int]] -> Int
day2Part2 = count (any safe . faces)