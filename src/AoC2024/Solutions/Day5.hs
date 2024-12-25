{-# LANGUAGE LambdaCase #-}
module AoC2024.Solutions.Day5 (parse, part1, part2) where 

import Data.List (partition)
import Control.Applicative (many)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import AoC2024.Parser (partialExecParser, char, natural, separatedBy)
import AoC2024.Utils (sumMap, middle)

forbiddenNumbers :: Ord a => [(a, a)] -> Map a (Set a)
forbiddenNumbers = Map.fromListWith Set.union . map (\(x, y) -> (y, Set.singleton x))

obeys :: Ord a => Map a (Set a) -> [a] -> Bool
obeys forbidden = loop Set.empty
  where
    loop fs = \case
      [] -> True
      x:xs | Set.member x fs -> False
           | otherwise -> loop (Map.findWithDefault Set.empty x forbidden <> fs) xs

tsort :: Ord a => Map a (Set a) -> [a] -> [a]
tsort forbidden xs 
  | null xs = []
  | otherwise = let
    (rest, start) = partition (\x -> any (`elem` xs) (Map.findWithDefault Set.empty x forbidden)) xs
    in start ++ tsort forbidden rest

parse :: String -> ([(Int, Int)], [[Int]])
parse = partialExecParser p
  where
    p = (,) <$> many restriction <* char '\n' <*> many update
    restriction = (,) <$> natural <* char '|' <*> natural <* char '\n'
    update = separatedBy (char ',') natural <* char '\n'

part1 :: ([(Int, Int)], [[Int]]) -> Int
part1 (restrictions, updates) = let
  forbidden = forbiddenNumbers restrictions
  obediant = filter (obeys forbidden) updates
  in sumMap middle obediant

part2 :: ([(Int, Int)], [[Int]]) -> Int
part2 (restrictions, updates) = let
  forbidden = forbiddenNumbers restrictions
  wrong = filter (not . obeys forbidden) updates
  in sumMap (middle . tsort forbidden) wrong