{-# LANGUAGE LambdaCase #-}
module AoC2024.Solutions.Day22 (parse, part1, part2) where

import Data.Bits (xor, (.&.), shiftR, shiftL)
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray, modifyArray', newArray)
import qualified Data.Map as Map
import AoC2024.Utils (freeze, nest, sumMap)

type Int4 = (Int, Int, Int, Int)

evolve :: Int -> Int
evolve n0 = let
  n1 = (n0 `xor` (n0 `shiftL` 6)) .&. 16777215
  n2 = (n1 `xor` (n1 `shiftR` 5)) .&. 16777215
  in (n2 `xor` (n2 `shiftL` 11)) .&. 16777215

changeSequences :: [Int] -> [(Int4, Int)]
changeSequences = \case
  x0:xs@(x1:x2:x3:x4:_) -> ((x1 - x0, x2 - x1, x3 - x2, x4 - x3), x4) : changeSequences xs
  _ -> []

addPayoffs :: STArray s Int4 Int -> [Int] -> ST s ()
addPayoffs arr xs = do
  let updates = Map.toList (Map.fromList (reverse (changeSequences xs)))
  forM_ updates $ \(changes, n) -> modifyArray' arr changes (+ n)

parse :: String -> [Int]
parse = map read . lines

part1 :: [Int] -> Int
part1 = sumMap (nest 2000 evolve)

part2 :: [Int] -> Int
part2 starts = runST $ do
  arr <- newArray ((-9,-9,-9,-9), (9,9,9,9)) 0
  forM_ starts $ \start -> do
    let secrets = take 2001 (iterate evolve start)
    addPayoffs arr (map (`mod` 10) secrets)
  maximum <$> freeze arr