{-# LANGUAGE LambdaCase #-}
module AoC2024.Utils where

import Data.Monoid (Sum(Sum), getSum)
import Data.Foldable (foldMap')
import Data.List (foldl')
import Data.Bifunctor (first)
import Control.Monad.ST (runST, ST)
import Control.Monad (forM_, filterM)
import Data.Array (Array, Ix, range, inRange, (!))
import Data.Array.ST (freeze, thaw, readArray, writeArray, getBounds, STArray)
import qualified Data.Array as Array
import Data.Array.Storable (modifyArray')

bezout :: Int -> Int -> (Int, Int, Int) 
bezout a b = bezout' (a, b) (1, 0) (0, 1)
  where
    bezout' (r0, r1) (s0, s1) (t0, t1)
      | r1 == 0 = (abs r0, s0 * signum r0, t0 * signum r0)
      | otherwise = 
        let q = r0 `div` r1 in
        bezout' (r1, r0 - q * r1) (s1, s0 - q * s1) (t1, t0 - q * t1)

-- More useful than sum
sumMap :: (Foldable t, Num b) => (a -> b) -> t a -> b
sumMap f = getSum . foldMap' (Sum . f)

applyN :: Int -> (a -> a) -> a -> a
applyN n f x
  | n == 0 = x
  | otherwise = f (applyN (n - 1) f x) 

require :: (a -> Bool) -> Maybe a -> Maybe a
require p = \case
  Nothing -> Nothing
  Just x -> if p x then Just x else Nothing

updateMap :: (c -> a -> (b, c)) -> c -> [a] -> [b]
updateMap f b = \case
  [] -> []
  x:xs -> let (x', b') = f b x in x' : updateMap f b' xs

takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM p = \case
  [] -> return []
  x:xs -> do
    px <- p x
    if px then (x:) <$> takeWhileM p xs else return []

dropWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
dropWhileM p = \case
  [] -> return []
  x:xs -> do
    px <- p x
    if px then dropWhileM p xs else return (x:xs)

spanM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
spanM p = \case
  [] -> return ([], [])
  x:xs -> do
    px <- p x
    if px then first (x:) <$> spanM p xs else return ([], x:xs)

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

type Grid = Array (Int, Int)

fromList :: [[a]] -> Grid a
fromList rows =
  let (m, n) = (length rows, length (head rows)) in
  Array.listArray ((0, 0), (m - 1, n - 1)) (concat rows)

dims :: Grid a -> (Int, Int)
dims = add2 (1, 1) . snd . Array.bounds

neighbors :: Grid a -> (Int, Int) -> [(Int, Int)]
neighbors grid i = filter (inRange (Array.bounds grid)) (neighbors' i)
  
neighborElems :: Grid a -> (Int, Int) -> [a]
neighborElems grid i = map (grid !) (neighbors grid i) 

-- Up, down, left, right
neighbors' :: (Int, Int) -> [(Int, Int)]
neighbors' (i, j) = [ (i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1) ]

neighborElems' :: Grid a -> (Int, Int) -> [Maybe a]
neighborElems' grid i = map (grid !?) (neighbors' i)

infixl 9 !?
(!?) :: Ix i => Array i a -> i -> Maybe a
(!?) arr i
  | inRange (Array.bounds arr) i = Just (arr ! i)
  | otherwise = Nothing

indexOf :: (Ix i, Eq a) => a -> Array i a -> i
indexOf x = fst . head . filter ((== x) . snd) . Array.assocs

mapWithIndex :: Ix i => (i -> a -> b) -> Array i a -> Array i b
mapWithIndex f arr =
  let assocs = map (\(i, x) -> (i, f i x)) (Array.assocs arr) in
  Array.array (Array.bounds arr) assocs

modifiedCopy :: Ix i => i -> a -> Array i a -> Array i a
modifiedCopy i x arr = runST $ do
  mut <- (thaw :: Ix i => Array i a -> ST s (STArray s i a)) arr
  writeArray mut i x
  freeze mut

modifyAll' :: Ix i => (a -> a) -> STArray s i a -> ST s ()
modifyAll' f arr = do
  ixs <- range <$> getBounds arr
  forM_ ixs $ \i -> modifyArray' arr i f

indicesWhere :: Ix i => (i -> a -> Bool) -> STArray s i a -> ST s [i]
indicesWhere p arr = do
  ixs <- range <$> getBounds arr
  filterM (\i -> p i <$> readArray arr i) ixs