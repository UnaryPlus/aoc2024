{-# LANGUAGE LambdaCase #-}
module AoC2024.Solutions.Day9 (parse, part1, part2) where

import Data.Char (digitToInt, isDigit)
import Data.List (foldl')
import Data.Bifunctor (first)
import Control.Monad (zipWithM, unless, forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray, readArray, writeArray, newListArray)
import AoC2024.Utils (sumMap, require, alternate, modifyAll', indicesWhereM, updateMap)

splitBlocksAt :: Int -> [(a, Int)] -> ([(a, Int)], [(a, Int)])
splitBlocksAt n xs
  | n <= 0 = ([], xs)
  | otherwise =
    case xs of
      [] -> ([], []) 
      (x, k):rest
        | k <= n -> first ((x, k):) (splitBlocksAt (n - k) rest)
        | otherwise -> ([(x, n)], (x, k - n):rest)

splitBlocks :: [Int] -> [(a, Int)] -> [[(a, Int)]]
splitBlocks ns xs = updateMap (\acc n -> splitBlocksAt n acc) xs ns

interleave :: [a] -> [[a]] -> [a]
interleave = \cases
  xs [] -> xs
  [] ys -> concat ys
  (x:xs) (y:ys) -> x : y ++ interleave xs ys

checkSumTerm :: (Int, Int, Int) -> Int
checkSumTerm (x, i, n) =
  let ixSum = n * i + n * (n - 1) `div` 2 in
  x * ixSum

checkSum :: [(Int, Int)] -> Int
checkSum = snd . foldl' loop (0, 0)
  where loop (i, total) (x, n) = (i + n, total + checkSumTerm (x, i, n))

getIndices :: Monad m => (Int -> m Int) -> (Int, Int) -> (Int, Int) -> m [Maybe Int]
getIndices = getIndices' []

-- Tail recursive
getIndices' :: Monad m => [Maybe Int] -> (Int -> m Int) -> (Int, Int) -> (Int, Int) -> m [Maybe Int]
getIndices' prev f (imin, imax) (lowest, highest)
  | imin > imax = return (prev ++ replicate (lowest - highest + 1) Nothing)
  | otherwise = do
    x <- f imin
    if x < lowest then getIndices' prev f (imin + 1, imax) (lowest, highest)
    else if x >= highest then return (prev ++ replicate (highest - lowest + 1) (Just imin))
    else getIndices' (prev ++ replicate (x - lowest + 1) (Just imin)) f (imin + 1, imax) (x + 1, highest)

-- Find space for a file among the first n gaps, and modify 'gaps' array if space is found
-- 'indices' array is for efficiency; indices[s] is the index of the first gap of size >= s
findSpace ::  STArray s Int (Maybe Int) -> STArray s Int (Int, Int) -> Int -> (Int, Int, Int) -> ST s (Int, Int, Int)
findSpace indices gaps n file@(fileID, _, fileSize) = do
  modifyAll' (require (< n)) indices
  gapIx <- readArray indices fileSize
  case gapIx of
    Nothing -> return file
    Just i -> do
      (gapStart, gapSize) <- readArray gaps i
      writeArray gaps i (gapStart + fileSize, gapSize - fileSize)
      needUpdating <- indicesWhereM (\fs j -> fs > gapSize - fileSize && j == Just i) indices
      unless (null needUpdating) $ do
        indices' <- getIndices (fmap snd . readArray gaps) (i + 1, n - 1) (minimum needUpdating, maximum needUpdating)
        let updates = zip needUpdating indices'
        forM_ updates (uncurry (writeArray indices))
      return (fileID, gapStart, fileSize)

findSpaceAll :: [(Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
findSpaceAll gaps files = runST $ do
  let numGaps = length gaps
  gapsArr <- newListArray (0, numGaps - 1) gaps
  indices <- getIndices (fmap snd . readArray gapsArr) (0, numGaps - 1) (1, 9)
  indicesArr <- newListArray (1, 9) indices
  let ns = map (numGaps -) [0..]
  zipWithM (findSpace indicesArr gapsArr) ns files

parse :: String -> ([Int], [Int])
parse = alternate . map digitToInt . takeWhile isDigit

part1 :: ([Int], [Int]) -> Int
part1 (fileSizes, gapSizes) = let
  totalFileSize = sum fileSizes
  files = zip [0..] fileSizes
  gapFillers = splitBlocks gapSizes (reverse files)
  (files', _) = splitBlocksAt totalFileSize (interleave files gapFillers)
  in checkSum files'

part2 :: ([Int], [Int]) -> Int
part2 (fileSizes, gapSizes) = let
  fileStarts = scanl (+) 0  (zipWith (+) fileSizes gapSizes) 
  gapStarts = scanl (+) (head fileSizes) (zipWith (+) (tail fileSizes) gapSizes)
  files = zip3 [0..] fileStarts fileSizes
  gaps = zip gapStarts gapSizes
  files' = findSpaceAll gaps (reverse files)
  in sumMap checkSumTerm files'