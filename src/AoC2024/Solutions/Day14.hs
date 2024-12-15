module AoC2024.Solutions.Day14 (parse, part1, part2) where

import Data.Foldable (foldMap')
import Data.Monoid (Sum(Sum))
import Data.Coerce (coerce)
import qualified Data.Set as Set
import AoC2024.Utils (argMax)
import AoC2024.Parser (Parser, partialExecParser, char, string, natural, integer)

type Pos = (Int, Int)
type Vel = (Int, Int)

elapse :: Int -> (Int, Int) -> (Pos, Vel) -> (Pos, Vel)
elapse n (m1, m2) ((p1, p2), (v1, v2))
  = (((p1 + n * v1) `mod` m1, (p2 + n * v2) `mod` m2), (v1, v2))

toQuadrant :: (Int, Int) -> Pos -> (Sum Int, Sum Int, Sum Int, Sum Int)
toQuadrant (m1, m2) (p1, p2) =
  case (compare (2 * p1) (m1 - 1), compare (2 * p2) (m2 - 1)) of
    (LT, LT) -> (1, 0, 0, 0)
    (LT, GT) -> (0, 1, 0, 0)
    (GT, LT) -> (0, 0, 1, 0)
    (GT, GT) -> (0, 0, 0, 1)
    _ -> (0, 0, 0, 0)

toQuadrants :: (Int, Int) -> [Pos] -> (Int, Int, Int, Int)
toQuadrants dims = coerce . foldMap' (toQuadrant dims)

symmetry :: (Int, Int) -> [Pos] -> Int
symmetry (m1, _) robots =
  length (Set.intersection (Set.fromList robots) (Set.fromList (map reflect robots)))
  where reflect (p1, p2) = (m1 - 1 - p1, p2)

parse :: String -> [(Pos, Vel)]
parse = map (partialExecParser p) . lines
  where
    p :: Parser (Pos, Vel)
    p = do
      pos <- (,) <$ string "p=" <*> natural <* char ',' <*> natural
      char ' '
      vel <- (,) <$ string "v=" <*> integer <* char ',' <*> integer
      return (pos, vel)

part1 :: [(Pos, Vel)] -> Int
part1 robots = let
  dims = (101, 103)
  (q1, q2, q3, q4) = toQuadrants dims (map (fst . elapse 100 dims) robots)
  in q1 * q2 * q3 * q4

part2 :: [(Pos, Vel)] -> Int
part2 robots = let
  dims = (101, 103)
  steps = take 10000 (iterate (map (elapse 1 dims)) robots)
  in argMax (map (symmetry dims . map fst) steps)
