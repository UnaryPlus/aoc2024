{-# LANGUAGE LambdaCase #-}
module AoC2024.Solutions.Day21 (parse, part1, part2) where

import AoC2024.Utils

data Key
  = KeyUp
  | KeyDown
  | KeyLeft
  | KeyRight
  | KeyA
  deriving (Eq, Show)

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

keyPosition :: Key -> (Int, Int)
keyPosition = \case
  KeyUp -> (0, 1)
  KeyA -> (0, 2)
  KeyLeft -> (1, 0)
  KeyDown -> (1, 1)
  KeyRight -> (1, 2)

-- (left, up) is better than (up, left) when possible
-- (left, down) is better than (down, left)
-- (down, right) is better than (right, down) when possible
-- But this only matters in the first step
keySpan :: (Int, Int) -> (Int, Int) -> [Key]
keySpan (r1, c1) (r2, c2) = (++ [KeyA]) $
  case (compare r1 r2, compare c1 c2) of
    (LT, LT)
      | c1 == 0 && r2 == 3 -> rights ++ downs
      | otherwise -> downs ++ rights
    (LT, EQ) -> downs
    (LT, GT) -> lefts ++ downs
    (EQ, LT) -> rights
    (EQ, EQ) -> []
    (EQ, GT) -> lefts
    (GT, LT) -> ups ++ rights
    (GT, EQ) -> ups
    (GT, GT)
      | r1 == 3 && c2 == 0 -> ups ++ lefts
      | otherwise -> lefts ++ ups
    where
      ups = replicate (r1 - r2) KeyUp
      lefts = replicate (c1 - c2) KeyLeft
      downs = replicate (r2 - r1) KeyDown
      rights = replicate (c2 - c1) KeyRight

keySequence :: [(Int, Int)] -> [Key]
keySequence = concat . mapPairs keySpan

controllingSequence :: String -> [Key]
controllingSequence nums = let
  seq1 = keySequence (map numPosition ('A' : nums))
  seq2 = keySequence (map keyPosition (KeyA : seq1))
  in keySequence (map keyPosition (KeyA : seq2))

parse :: String -> [String]
parse = lines

part1 :: [String] -> Int
part1 = sumMap complexity
  where complexity str = read (init str) * length (controllingSequence str)

part2 :: [String] -> Int
part2 = undefined