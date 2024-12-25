module AoC2024.Solutions.Day25 (parse, part1) where

import Data.List (transpose, partition)
import Control.Category ((>>>))
import AoC2024.Utils (chunksOf, count, mapBoth)

type Lock = [Int]
type Key = [Int]

noOverlap :: Lock -> Key -> Bool
noOverlap lock key = all (\(x, y) -> x + y <= 5) (zip lock key)

parse :: String -> ([Lock], [Key])
parse = lines
  >>> filter (not . null)
  >>> chunksOf 7
  >>> map transpose
  >>> partition ((== '#') . head . head)
  >>> mapBoth (map (map (\col -> count (== '#') col - 1)))

part1 :: ([Lock], [Key]) -> Int
part1 (locks, keys) = 
  count id [ noOverlap lock key | lock <- locks, key <- keys ]
