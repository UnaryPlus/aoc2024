module AoC2024.Solutions.Day13 (parse, part1, part2) where

import Data.Maybe (mapMaybe)
import Data.Bifunctor (bimap, second)
import AoC2024.Utils (sumMap)
import AoC2024.Parser (Parser, partialExecParser, char, natural, separatedBy, string, eof)

type Matrix = (Int, Int, Int, Int)

det :: Matrix -> Int
det (a1, a2, b1, b2) = a1 * b2 - a2 * b1

-- Fortunately, all the matrices in the input file are invertible,
-- so we just need to check whether the unique solution is integral
solve :: Matrix -> (Int, Int) -> Maybe (Int, Int)
solve matrix@(a1, a2, b1, b2) (c1, c2) = let
  d = det matrix
  (x, mx) = divMod (c1 * b2 - c2 * b1) d
  (y, my) = divMod (a1 * c2 - a2 * c1) d
  in if mx == 0 && my == 0 then Just (x, y) else Nothing

cost :: (Int, Int) -> Int
cost (x, y) = 3 * x + y

parse :: String -> [(Matrix, (Int, Int))]
parse = partialExecParser (separatedBy (char '\n') p <* eof)
  where 
    p :: Parser (Matrix, (Int, Int)) 
    p = do
      mat <- (,,,) 
        <$ string "Button A: X+" <*> natural <* string ", Y+" <*> natural <* char '\n'
        <* string "Button B: X+" <*> natural <* string ", Y+" <*> natural <* char '\n'
      vec <- (,) 
        <$ string "Prize: X=" <*> natural <* string ", Y=" <*> natural <* char '\n'
      return (mat, vec)

part1 :: [(Matrix, (Int, Int))] -> Int
part1 eqns = sumMap cost (mapMaybe (uncurry solve) eqns)

part2 :: [(Matrix, (Int, Int))] -> Int
part2 eqns =
  let eqns' = map (second (bimap (+10000000000000) (+10000000000000))) eqns
  in sumMap cost (mapMaybe (uncurry solve) eqns')