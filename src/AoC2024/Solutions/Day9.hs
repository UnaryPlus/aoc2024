{-# LANGUAGE LambdaCase #-}

module AoC2024.Solutions.Day9 (parse, part1, part2) where

import Data.Char (digitToInt, isDigit)
import Data.List (foldl')
import Data.Bifunctor (first)
import Control.Monad (zipWithM)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray, readArray, writeArray, newArray, freeze, thaw, getBounds)
import Data.Array (Array, range, (!))
import qualified Data.Array as Array
import AoC2024.Utils (alternate, updateMap, dropWhileM)

import Debug.Trace

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
splitBlocks [] _ = []
splitBlocks (n:ns) xs =
  let (taken, dropped) = splitBlocksAt n xs 
  in taken : splitBlocks ns dropped

interleave :: [a] -> [[a]] -> [a]
interleave = curry $ \case
  (xs, []) -> xs
  ([], ys) -> concat ys
  (x:xs, y:ys) -> x : y ++ interleave xs ys

checkSumTerm :: (Int, Int, Int) -> Int
checkSumTerm (x, i, n) =
  let ixSum = n * i + n * (n - 1) `div` 2 in
  x * ixSum

checkSum :: [(Int, Int)] -> Int
checkSum = snd . foldl' loop (0, 0)
  where loop (i, total) (x, n) = (i + n, total + checkSumTerm (x, i, n))

findSpace :: STArray s Int (Int, Int) -> Int -> (Int, Int, Int) -> ST s (Int, Int, Int)
findSpace gaps n file@(fileID, _, fileSize) = do
  traceShowM n
  gapIxs <- take n . range <$> getBounds gaps
  rest <- dropWhileM (fmap ((< fileSize) . snd) . readArray gaps) gapIxs
  case rest of
    [] -> return file -- All gaps too small
    i:_ -> do
      (gapStart, gapSize) <- readArray gaps i
      writeArray gaps i (gapStart + fileSize, gapSize - fileSize)
      return (fileID, gapStart, fileSize)

findSpaceAll :: [(Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
findSpaceAll gaps files = runST $ do
  let numGaps = length gaps
  gaps' <- thaw (Array.listArray (0, numGaps - 1) gaps)
  let ns = map (numGaps -) [0..]
  zipWithM (findSpace gaps') ns files

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
  in sum (map checkSumTerm files')