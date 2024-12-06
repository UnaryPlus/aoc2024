{-# LANGUAGE LambdaCase #-}

module AoC2024.Utils where

import Data.List (foldl')
import Control.Monad.ST (runST, ST)
import Data.Array (Array, Ix, inRange, (!))
import Data.Array.ST (freeze, thaw, writeArray, STArray)
import qualified Data.Array as Array

alternate :: [a] -> ([a], [a])
alternate = \case
  [] -> ([], [])
  [x] -> ([x], [])
  x:y:zs -> let (xs, ys) = alternate zs in (x:xs, y:ys)

count :: Foldable t => (a -> Bool) -> t a -> Int
count p = foldl' (\n x -> if p x then n + 1 else n) 0 

differences :: Num a => [a] -> [a]
differences = \case
  [] -> []
  [_] -> []
  x:xs@(y:_) -> (y - x) : differences xs

infixr 3 &&&
(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) p q x = p x && q x

infixr 2 |||
(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) p q x = p x || q x

-- Name comes from thinking of the list as a simplex
faces :: [a] -> [[a]]
faces [] = []
faces (x:xs) = xs : map (x:) (faces xs)

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

add2 :: Num a => (a, a) -> (a, a) -> (a, a)
add2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

type Grid a = Array (Int, Int) a

fromList :: [[a]] -> Grid a
fromList rows =
  let (m, n) = (length rows, length (head rows)) in
  Array.listArray ((0, 0), (m - 1, n - 1)) (concat rows)

dims :: Grid a -> (Int, Int)
dims = add2 (1, 1) . snd . Array.bounds

safeAccess :: Ix i => i -> Array i a -> Maybe a
safeAccess i arr
  | inRange (Array.bounds arr) i = Just (arr ! i)
  | otherwise = Nothing

indexOf :: (Ix i, Eq a) => a -> Array i a -> i
indexOf x = fst . head . filter ((== x) . snd) . Array.assocs

modifiedCopy :: Ix i => i -> a -> Array i a -> Array i a
modifiedCopy i x arr = runST $ do
  mut <- (thaw :: Ix i => Array i a -> ST s (STArray s i a)) arr
  writeArray mut i x
  freeze mut