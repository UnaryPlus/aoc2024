{-# LANGUAGE LambdaCase #-}
module AoC2024.Solutions.Day24 (parse, part1, part2) where

import Data.List (permutations, sort)
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe)
import Control.Applicative (many, (<|>), liftA2)
import qualified Data.Map as Map
import AoC2024.Parser (partialExecParser, char, string, eof, parseWhile)
import AoC2024.Utils ((!), (!?), sumMap, commaJoin, hasCycle)

data Operation = And | Or | Xor
  deriving (Eq, Show)

apply :: Operation -> Bool -> Bool -> Bool
apply = \case
  And -> (&&)
  Or -> (||)
  Xor -> (/=)

fromBits :: [Bool] -> Int
fromBits = sumMap fst . filter snd . zip powers
  where powers = map (2^) [(0 :: Int)..]

swapOutputs :: Eq a => [(a, a)] -> [(a, b, c, d)] -> [(a, b, c, d)]
swapOutputs pairs = 
  map (\(x0, op, x1, x2) -> (swap x0, op, x1, x2))
  where 
    swap x
      | [x'] <- map snd (filter ((== x) . fst) pairs) = x'
      | [x'] <- map fst (filter ((== x) . snd) pairs) = x'
      | otherwise = x

loopsForever :: [(String, b, String, String)] -> Bool
loopsForever gates = let
  graph = Map.fromList (map (\(x0, _, x1, x2) -> (x0, [x1, x2])) gates) 
  zs = filter ((== 'z') . head) (Map.keys graph)
  in any (hasCycle (\x -> fromMaybe [] (graph !? x))) zs

parse :: String -> ([(String, Bool)], [(String, Operation, String, String)])
parse = partialExecParser p
  where 
    p = (,) <$> many start <* char '\n' <*> many gate <* eof
    start = (,) <$> parseWhile isAlphaNum <* string ": " <*> bit <* char '\n'
    bit = (False <$ char '0') <|> (True <$ char '1')
    gate = (\x1 op x2 x0 -> (x0, op, x1, x2))
      <$> parseWhile isAlphaNum <* char ' ' <*> operation <* char ' ' <*> parseWhile isAlphaNum 
      <* string " -> " <*> parseWhile isAlphaNum <* char '\n'
    operation = (And <$ string "AND") <|> (Or <$ string "OR") <|> (Xor <$ string "XOR")

part1 :: ([(String, Bool)], [(String, Operation, String, String)]) -> Int
part1 (starts, gates) = let
  values = Map.fromList (starts ++ map (\(x0, op, x1, x2) -> (x0, apply op (values ! x1) (values ! x2))) gates)
  zBits = map snd (filter ((== 'z') . head . fst) (Map.toAscList values))
  in fromBits zBits

-- Structure of 45-bit binary adder:
-- 89 XOR gates, 89 AND gates, 44 OR gates
-- Outputs of XOR gates: 
--   45 are in {z0, ..., z44}
--   44 are inputs of a XOR gate and an AND gate
-- Outputs of AND gates:
--   1 is input of a XOR gate and an AND gate
--   88 are inputs of an OR gate
-- Outputs of OR gates:
--   1 is z45
--   43 are inputs of a XOR gate and an AND gate

-- Structure of input circuit:
-- Outputs of XOR gates:
--   42 are in {z0, ..., z44}
--   44 are inputs of a XOR gate and an AND gate
--   3 (djg, sbg, mcq) are inputs of an OR gate
-- Outputs of AND gates:
--   2 (ktt, hjm) are inputs of a XOR gate and an AND gate
--   85 are inputs of an OR gate
--   2 (z12, z19) are in {z0, ..., z44}
-- Outputs of OR gates:
--   1 is z45
--   42 are inputs of a XOR gate and an AND gate
--   1 (z37) is in {z0, ..., z44}

-- Conclusion:
-- {djg, sbg, mcq} swap with {z12, z19, ktt or hjm}
-- something swaps with z37
part2 :: ([(String, Bool)], [(String, Operation, String, String)]) -> String
part2 (starts, gates) = let
  xorOutputs = [ x | (x, Xor, _, _) <- gates ]
  toWhich x = [ op | (_, op, x1, x2) <- gates, x1 == x || x2 == x ]
  candidates = filter (\x -> toWhich x `elem` [[Xor, And], [And, Xor]]) xorOutputs
  swaps1 = [ zip ["djg", "sbg", "mcq"] perm | x <- ["ktt", "hjm"], perm <- permutations ["z12", "z19", x] ]
  swaps2 = [ [("z37", x)] | x <- candidates ] 
  swaps = liftA2 (++) swaps1 swaps2
  result = 
    let (xs, ys) = splitAt 45 starts in
    fromBits (map snd xs) + fromBits (map snd ys)
  successful = flip filter swaps $ \pairs -> 
    let gates' = swapOutputs pairs gates in
    not (loopsForever gates')
    && part1 (starts, gates') == result
  involved = concatMap (\(x, y) -> [x, y]) (head successful)
  in commaJoin (sort involved)