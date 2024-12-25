module AoC2024.Solutions.Day23 (parse, part1, part2) where

import Data.Map (Map)
import Data.Char (isAsciiLower)
import Control.Category ((>>>))
import Control.Monad (guard)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import AoC2024.Parser (partialExecParser, parseWhile, char, eof)
import AoC2024.Utils (choose2, count, (!), commaJoin) 

triangles :: Ord a => Map a (Set a) -> Set (Set a)
triangles graph = Set.fromList $ do
  (x1, nbrs) <- Map.toList graph
  (x2, x3) <- choose2 (Set.toList nbrs)
  guard (Set.member x3 (graph ! x2))
  return (Set.fromList [x1, x2, x3])

parse :: String -> Map String (Set String)
parse = lines
  >>> concatMap (partialExecParser p >>> \(x, y) -> [ (x, [y]), (y, [x]) ])
  >>> Map.fromListWith (++)
  >>> fmap Set.fromList
  where p = (,) <$> parseWhile isAsciiLower <* char '-' <*> parseWhile isAsciiLower <* eof

part1 :: Map String (Set String) -> Int
part1 = count (any ((== 't') . head)) . triangles

-- Properties of graph:
-- 520 vertices
-- Connected
-- Every vertex has degree 13
-- Every vertex is contained in either 55, 65, or 66 triangles

-- There are 13 vertices of the third type; since 66 = 12 choose 2,
-- I suspected that these vertices constitute a clique.
-- If they do constitute a clique, it must be the largest clique,
-- because if there were a clique of size 14, it would be disconnected 
-- from the rest of the graph.
part2 :: Map String (Set String) -> String
part2 graph = let
  tris = triangles graph
  clique = Set.filter (\x -> count (Set.member x) tris == 66) (Map.keysSet graph)
  in commaJoin (Set.toAscList clique)
