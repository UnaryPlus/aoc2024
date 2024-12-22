{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
module AoC2024.Utils where

import Data.Monoid (Sum(Sum), getSum)
import Data.Foldable (foldMap')
import Data.List (foldl', minimumBy, maximumBy)
import Data.Function (on)
import Data.Bifunctor (first)
import Control.Monad.ST (runST, ST)
import Control.Monad (forM_, filterM)
import Data.Ix (Ix, range, inRange)
import Data.Array.ST (readArray, writeArray, modifyArray', getBounds, STArray)
import qualified Data.Array.MArray as MArray
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified AoC2024.Utils.PSQueue as PSQueue

class Indexed t i | t -> i where
  infixl 9 !, !?
  (!) :: t a -> i -> a
  (!?) :: t a -> i -> Maybe a
  assocs :: t a -> [(i, a)]
  mapWithIndex :: (i -> a -> b) -> t a -> t b

instance Indexed [] Int where
  (!) :: [a] -> Int -> a
  (!) = (!!)

  (!?) :: [a] -> Int -> Maybe a
  (!?) = \cases
    _ n | n < 0 -> Nothing
    [] _ -> Nothing
    (x:xs) n 
      | n == 0 -> Just x
      | otherwise -> xs !? (n - 1) 
  
  assocs :: [a] -> [(Int, a)]
  assocs = zip [0..]

  mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
  mapWithIndex f = map (uncurry f) . assocs

instance Ix i => Indexed (Array i) i where
  (!) :: Ix i => Array i a -> i -> a
  (!) = (Array.!)

  (!?) :: Ix i => Array i a -> i -> Maybe a
  (!?) arr i
    | inRange (Array.bounds arr) i = Just (arr ! i)
    | otherwise = Nothing

  assocs :: Ix i => Array i a -> [(i, a)]
  assocs = Array.assocs

  mapWithIndex :: Ix i => (i -> a -> b) -> Array i a -> Array i b
  mapWithIndex f arr =
    let assocs' = map (\(i, x) -> (i, f i x)) (assocs arr) in
    Array.array (Array.bounds arr) assocs'

instance Ord k => Indexed (Map k) k where
  (!) :: Ord k => Map k a -> k -> a
  (!) = (Map.!)  

  (!?) :: Ord k => Map k a -> k -> Maybe a
  (!?) = (Map.!?)

  assocs :: Ord k => Map k a -> [(k, a)]
  assocs = Map.toList
  
  mapWithIndex :: Ord k => (k -> a -> b) -> Map k a -> Map k b
  mapWithIndex = Map.mapWithKey

indicesWhere :: Indexed t i => (a -> Bool) -> t a -> [i]
indicesWhere p = map fst . filter (p . snd) . assocs

indexWhere :: Indexed t i => (a -> Bool) -> t a -> i
indexWhere p = head . indicesWhere p

indicesOf :: (Indexed t i, Eq a) => a -> t a -> [i]
indicesOf x = indicesWhere (== x)

indexOf :: (Indexed t i, Eq a) => a -> t a -> i
indexOf x = head . indicesOf x

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

nest :: Int -> (a -> a) -> a -> a
nest n f x
  | n == 0 = x
  | otherwise = f (nest (n - 1) f x) 

nestM :: Monad m => Int -> (a -> m a) -> a -> m a
nestM n f x 
  | n == 0 = f x
  | otherwise = f =<< nestM (n - 1) f x

whileJust :: (a -> Maybe a) -> a -> a
whileJust f x =
  case f x of
    Nothing -> x
    Just x' -> whileJust f x'

untilLeft :: (a -> Either b a) -> a -> b
untilLeft f x =
  case f x of
    Left y -> y
    Right x' -> untilLeft f x'

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

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | null xs = []
  | otherwise = take n xs : chunksOf n (drop n xs)

dropPrefix :: Eq a => [a] -> [a] -> Maybe [a]
dropPrefix = \cases
  [] xs -> Just xs
  (p:ps) (x:xs) | p == x -> dropPrefix ps xs
  _ _ -> Nothing

count :: Foldable t => (a -> Bool) -> t a -> Int
count p = foldl' (\n x -> if p x then n + 1 else n) 0 

mapPairs :: (a -> a -> b) -> [a] -> [b]
mapPairs f = \case
  [] -> []
  [_] -> []
  x:xs@(y:_) -> f x y : mapPairs f xs

differences :: Num a => [a] -> [a]
differences = mapPairs (flip (-))

argMin :: Ord a => [a] -> Int
argMin = fst . foldl1 (\(i, x) (j, y) -> if x <= y then (i, x) else (j, y)) . zip [0..]

argMax :: Ord a => [a] -> Int
argMax = fst . foldl1 (\(i, x) (j, y) -> if x >= y then (i, x) else (j, y)) . zip [0..]

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f = minimumBy (compare `on` f)

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = maximumBy (compare `on` f)

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

type Array2 = Array (Int, Int)
type STArray2 s = STArray s (Int, Int)

fromList :: [[a]] -> Array2 a
fromList rows =
  let (m, n) = (length rows, length (head rows)) in
  Array.listArray ((0, 0), (m - 1, n - 1)) (concat rows)

toList :: Array2 a -> [[a]]
toList grid = chunksOf (snd (dims grid)) (Array.elems grid)

dims :: Array2 a -> (Int, Int)
dims = add2 (1, 1) . snd . Array.bounds

up, down, left, right :: Num a => (a, a)
up = (-1, 0)
down = (1, 0)
left = (0, -1)
right = (0, 1)

cardinalDirections :: Num a => [(a, a)]
cardinalDirections = [ up, down, left, right ]

turnLeft, turnRight :: Num a => (a, a) -> (a, a)
turnLeft (x, y) = (-y, x)
turnRight (x, y) = (y, -x)

neighbors :: Array2 a -> (Int, Int) -> [(Int, Int)]
neighbors grid i = filter (inRange (Array.bounds grid)) (neighbors' i)
  
neighborElems :: Array2 a -> (Int, Int) -> [a]
neighborElems grid i = map (grid !) (neighbors grid i) 

manhattanCircle :: Array2 a -> Int -> (Int, Int) -> [(Int, Int)]
manhattanCircle grid r i = filter (inRange (Array.bounds grid)) (manhattanCircle' r i)

manhattanBall :: Array2 a -> Int -> (Int, Int) -> [(Int, Int)]
manhattanBall grid r i = filter (inRange (Array.bounds grid)) (manhattanBall' r i)

neighbors' :: (Int, Int) -> [(Int, Int)]
neighbors' (i, j) = map (add2 (i, j)) cardinalDirections 

neighborElems' :: Array2 a -> (Int, Int) -> [Maybe a]
neighborElems' grid i = map (grid !?) (neighbors' i)

manhattanCircle' :: Int -> (Int, Int) -> [(Int, Int)]
manhattanCircle' r (i, j)
  | r == 0 = [ (i, j) ]
  | otherwise = topLeft ++ topRight ++ bottomLeft ++ bottomRight
  where
    topLeft = [ (i - r + k, j - k) | k <- [0..r-1] ]
    topRight = [ (i - r + k, j + k) | k <- [1..r] ]
    bottomLeft = [ (i + r - k, j - k) | k <- [1..r] ]
    bottomRight = [ (i + r - k, j + k) | k <- [0..r-1] ]

manhattanBall' :: Int -> (Int, Int) -> [(Int, Int)]
manhattanBall' r i = concatMap (\r' -> manhattanCircle' r' i) [0..r]

trueArray :: Ix i => (i, i) -> [i] -> Array i Bool
trueArray bounds trues = Array.accumArray (||) False bounds (map (, True) trues)

falseArray :: Ix i => (i, i) -> [i] -> Array i Bool
falseArray bounds falses = Array.accumArray (&&) True bounds (map (, False) falses)

freeze :: Ix i => STArray s i a -> ST s (Array i a)
freeze = MArray.freeze

thaw :: Ix i => Array i a -> ST s (STArray s i a)
thaw = MArray.thaw

modifiedCopy :: Ix i => i -> a -> Array i a -> Array i a
modifiedCopy i x arr = runST $ do
  mut <- thaw arr
  writeArray mut i x
  freeze mut

modifyAll' :: Ix i => (a -> a) -> STArray s i a -> ST s ()
modifyAll' f arr = do
  ixs <- range <$> getBounds arr
  forM_ ixs $ \i -> modifyArray' arr i f

indicesWhereM :: Ix i => (i -> a -> Bool) -> STArray s i a -> ST s [i]
indicesWhereM p arr = do
  ixs <- range <$> getBounds arr
  filterM (\i -> p i <$> readArray arr i) ixs

graphDistances :: (Foldable t, Ord a, Ord d, Num d) => (a -> t (a, d)) -> a -> Map a d
graphDistances getNeighbors start = fst <$>
  generalDijkstra (const ()) (\_ _ _ -> ()) (\_ _ -> ()) getNeighbors start

graphDistance :: (Foldable t, Ord a, Ord d, Num d) => (a -> t (a, d)) -> a -> a -> Maybe d
graphDistance getNeighbors start end = fst <$> 
  generalDijkstra1 (const ()) (\_ _ _ -> ()) (\_ _ -> ()) getNeighbors start end

-- Dijkstra's shortest path algorithm
-- Returns data structure mapping vertex v to (d(v), m(v)) where:
--     d(v) is the shortest distance from start to v
--     m(v) is some data concerning the shortest path(s) from start to v
generalDijkstra :: (Foldable t, Ord a, Ord d, Num d) => (a -> m) -> (a -> a -> m -> m) -> (m -> m -> m) -> (a -> t (a, d)) -> a -> Map a (d, m)
generalDijkstra startData makeData combineData getNeighbors start =
  loop Map.empty (PSQueue.singleton start 0 (startData start))
  where    
    loop certain uncertain
      | PSQueue.null uncertain = certain
      | otherwise = let
        (node, dist, info) = PSQueue.peek uncertain
        insert (nbr, d)
          | Map.member nbr certain = id
          | otherwise = PSQueue.insertWith combineData nbr (dist + d) (makeData nbr node info)
        certain' = Map.insert node (dist, info) certain
        uncertain' = foldr insert (PSQueue.delete node uncertain) (getNeighbors node)
        in loop certain' uncertain'

generalDijkstra1 :: (Foldable t, Ord a, Ord d, Num d) => (a -> m) -> (a -> a -> m -> m) -> (m -> m -> m) -> (a -> t (a, d)) -> a -> a -> Maybe (d, m)
generalDijkstra1 startData makeData combineData getNeighbors start end =
  loop Set.empty (PSQueue.singleton start 0 (startData start))
  where      
    loop certain uncertain
      | PSQueue.null uncertain = Nothing
      | otherwise = let
        (node, dist, info) = PSQueue.peek uncertain
        insert (nbr, d)
          | Set.member nbr certain = id
          | otherwise = PSQueue.insertWith combineData nbr (dist + d) (makeData nbr node info)
        certain' = Set.insert node certain
        uncertain' = foldr insert (PSQueue.delete node uncertain) (getNeighbors node)
        in if node == end then Just (dist, info) else loop certain' uncertain'