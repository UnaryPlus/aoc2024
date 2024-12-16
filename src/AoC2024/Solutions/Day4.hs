module AoC2024.Solutions.Day4 (parse, part1, part2) where 

import qualified Data.Array as Array
import AoC2024.Utils ((!), count, (|||), add2, Array2, fromList, dims)

applyMask :: Functor f => (Int, Int) -> f (Int, Int) -> Array2 a -> f a
applyMask base mask grid = fmap (\ix -> grid ! add2 base ix) mask

slideMask :: Functor f => (Int, Int) -> (Int, Int) -> f (Int, Int) -> Array2 a -> [f a]
slideMask (i0, j0) (i1, j1) mask grid = [ applyMask (i, j) mask grid | i <- [i0..i1], j <- [j0..j1] ]

slideMasks :: Functor f => [((Int, Int), (Int, Int), f (Int, Int))] -> Array2 a -> [f a]
slideMasks masks grid = concatMap (\(ix0, ix1, mask) -> slideMask ix0 ix1 mask grid) masks

parse :: String -> Array2 Char
parse = fromList . lines

part1 :: Array2 Char -> Int
part1 grid = let
  (m, n) = dims grid
  masks = 
    [ ((0, 0), (m - 1, n - 4), [(0, 0), (0, 1), (0, 2), (0, 3)]) 
    , ((0, 0), (m - 4, n - 1), [(0, 0), (1, 0), (2, 0), (3, 0)])
    , ((0, 0), (m - 4, n - 4), [(0, 0), (1, 1), (2, 2), (3, 3)])
    , ((0, 0), (m - 4, n - 4), [(0, 3), (1, 2), (2, 1), (3, 0)])
    ]
  in count ((== "XMAS") ||| (== "SAMX")) (slideMasks masks grid)

part2 :: Array2 Char -> Int
part2 grid = let
  (m, n) = dims grid
  mask = Array.listArray (0 :: Int, 4) [(-1, -1), (-1, 1), (0, 0), (1, -1), (1, 1)]
  exes = slideMask (1, 1) (m - 2, n - 2) mask grid
  ms = [('M', 'S'), ('S', 'M')]
  isXmas ex = ex ! 2 == 'A' && (ex ! 0, ex ! 4) `elem` ms && (ex ! 1, ex ! 3) `elem` ms
  in count isXmas exes