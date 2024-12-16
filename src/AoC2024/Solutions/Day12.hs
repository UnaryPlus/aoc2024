module AoC2024.Solutions.Day12 (parse, part1, part2) where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.Map as Map
import Data.Array.ST (newArray, readArray, writeArray, freeze)
import Data.Ix (range)
import qualified Data.Array as Array
import AoC2024.Utils (sumMap, count, Array2, STArray2, fromList, neighbors, neighborElems', mapWithIndex, assocs, (!), (!?))

adjacentFences :: Eq a => Array2 a -> Array2 Int
adjacentFences grid = 
  mapWithIndex (\i c -> count (/= Just c) (neighborElems' grid i)) grid

-- "side end" = top of vertical side or left of horizontal side
adjacentSideEnds :: Eq a => Array2 a -> Array2 Int
adjacentSideEnds grid =
  mapWithIndex (\(i, j) c -> let
    up = grid !? (i - 1, j)
    down = grid !? (i + 1, j)
    left = grid !? (i, j - 1)
    right = grid !? (i, j + 1)
    upLeft = grid !? (i - 1, j - 1)
    upRight = grid !? (i - 1, j + 1)
    downLeft = grid !? (i + 1, j - 1)
    in count id 
      [ up /= Just c && (left /= Just c || upLeft == Just c)
      , down /= Just c && (left /= Just c || downLeft == Just c)
      , left /= Just c && (up /= Just c || upLeft == Just c)
      , right /= Just c && (up /= Just c || upRight == Just c)
      ]
  ) grid

markRegion :: Eq a => Array2 a -> STArray2 s Int -> Int -> (Int, Int) -> ST s ()
markRegion grid markers marker i = do
  explored <- readArray markers i
  when (explored == (-1)) $ do
    writeArray markers i marker
    let adj = filter (\j -> grid ! i == grid ! j) (neighbors grid i)
    mapM_ (markRegion grid markers marker) adj

markRegions :: Eq a => Array2 a -> Array2 Int
markRegions grid = runST $ do
  markers <- newArray (Array.bounds grid) (-1)
  counter <- newSTRef 0
  forM_ (range (Array.bounds grid)) $ \i -> do
    explored <- readArray markers i
    when (explored == (-1)) $ do
      marker <- readSTRef counter
      markRegion grid markers marker i
      modifySTRef' counter (+1)
  freeze markers

getRegions :: Eq a => Array2 a -> [[(Int, Int)]]
getRegions = Map.elems . Map.fromListWith (++) . map (\(i, n) -> (n, [i])) . assocs . markRegions

parse :: String -> Array2 Char
parse = fromList . lines

part1 :: Array2 Char -> Int
part1 grid = let
  fences = adjacentFences grid
  regions = getRegions grid
  cost r = length r * sumMap (fences !) r
  in sumMap cost regions

part2 :: Array2 Char -> Int
part2 grid = let
  sideEnds = adjacentSideEnds grid
  regions = getRegions grid
  cost r = length r * sumMap (sideEnds !) r
  in sumMap cost regions