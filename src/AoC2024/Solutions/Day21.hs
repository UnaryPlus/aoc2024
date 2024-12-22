{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module AoC2024.Solutions.Day21 where -- (parse, part1, part2) where

import Data.Ix
import Data.Array (Array)
import qualified Data.Array as Array
import AoC2024.Utils
import Data.Array.ST
import Control.Monad.ST

data Key = U | D | L | R | A
  deriving (Eq, Show)

-- Only chunks left after applying lift twice
data Chunk
  = AA
  | AUA
  | ADA
  | ALA
  | ARA
  | ALUA
  | ALDA
  | ARUA
  | ADLLA
  | ARRUA
  deriving (Eq, Ord, Enum, Bounded, Show, Ix)

sizes :: Array Chunk Int
sizes = Array.array (minBound, maxBound)
  [ (AA, 1)
  , (AUA, 2), (ADA, 2), (ALA, 2), (ARA, 2)
  , (ALUA, 3), (ALDA, 3), (ARUA, 3)
  , (ADLLA, 4), (ARRUA, 4) 
  ]

liftChunk :: Chunk -> [Chunk]
liftChunk = \case
  AA -> [ AA ]
  AUA -> [ ALA, ARA ]
  ADA -> [ ALDA, ARUA ]
  ALA -> [ ADLLA, ARRUA ]
  ARA -> [ ADA, AUA ]
  ALUA -> [ ADLLA, ARUA, ARA ] 
  ALDA -> [ ADLLA, ARA, ARUA ]
  ARUA -> [ ADA, ALUA, ARA ]
  ADLLA -> [ ALDA, ALA, AA, ARRUA ] 
  ARRUA -> [ ADA, AA, ALUA, ARA ]

origins :: Chunk -> [Chunk]
origins = \case
  AA -> [ AA, ADLLA, ARRUA ]
  AUA -> [ ARA ]
  ADA -> [ ARA, ARUA, ARRUA ]
  ALA -> [ AUA, ADLLA ]
  ARA -> [ AUA, ALUA, ALDA, ARUA, ARRUA ]
  ALUA -> [ ARUA, ARRUA ]
  ALDA -> [ ADA, ADLLA ]
  ARUA -> [ ADA, ALUA, ALDA ]
  ADLLA -> [ ALA, ALUA, ALDA ]
  ARRUA -> [ ALA, ADLLA ]

liftChunks :: Array Chunk Int -> Array Chunk Int
liftChunks arr = 
  Array.array (minBound, maxBound)
  [ (c, sumMap (arr !) (origins c)) | c <- [minBound..maxBound] ]

readChunk :: [Key] -> Chunk
readChunk = \case
  [] -> AA
  [U] -> AUA
  [D] -> ADA
  [L] -> ALA
  [R] -> ARA
  [L,U] -> ALUA
  [L,D] -> ALDA
  [R,U] -> ARUA
  [D,L,L] -> ADLLA
  [R,R,U] -> ARRUA
  _ -> undefined

readChunksST :: STArray s Chunk Int -> [Key] -> ST s ()
readChunksST arr keys
  | null keys = return ()
  | otherwise = do
    let (left, tail -> right) = break (== A) keys
    modifyArray' arr (readChunk left) (+1)
    readChunksST arr right

readChunks :: [Key] -> Array Chunk Int
readChunks keys = runST $ do
  arr <- newArray (minBound, maxBound) 0
  readChunksST arr keys
  freeze arr

numPosition :: Char -> (Int, Int)
numPosition = \case
  '7' -> (0, 0)
  '8' -> (0, 1)
  '9' -> (0, 2)
  '4' -> (1, 0)
  '5' -> (1, 1)
  '6' -> (1, 2)
  '1' -> (2, 0)
  '2' -> (2, 1)
  '3' -> (2, 2)
  '0' -> (3, 1)
  'A' -> (3, 2)
  _ -> undefined

dirPosition :: Key -> (Int, Int)
dirPosition = \case
  U -> (0, 1)
  A -> (0, 2)
  L -> (1, 0)
  D -> (1, 1)
  R -> (1, 2)

-- (left, up) is better than (up, left) when possible
-- (left, down) is better than (down, left) when possible
-- (down, right) is better than (right, down) when possible
keySpan :: (Int, Int) -> (Int, Int) -> [Key]
keySpan (r1, c1) (r2, c2) = (++ [A]) $
  case (compare r1 r2, compare c1 c2) of
    (LT, LT)
      | c1 == 0 && r2 == 3 -> rights ++ downs
      | otherwise -> downs ++ rights
    (LT, EQ) -> downs
    (LT, GT) -> lefts ++ downs
    (EQ, LT) -> rights
    (EQ, EQ) -> []
    (EQ, GT) -> lefts
    (GT, LT) -> ups ++ rights
    (GT, EQ) -> ups
    (GT, GT)
      | r1 == 3 && c2 == 0 -> ups ++ lefts
      | otherwise -> lefts ++ ups
    where
      ups = replicate (r1 - r2) U
      lefts = replicate (c1 - c2) L
      downs = replicate (r2 - r1) D
      rights = replicate (c2 - c1) R

keySequence :: [(Int, Int)] -> [Key]
keySequence = concat . mapPairs keySpan

-- TODO: rename
keySpan' :: Key -> Key -> [Key]
keySpan' key1 key2
  | (key1, key2) `elem` [ (D, U), (R, A) ] = [ U, A ]
  | (key1, key2) `elem` [ (U, D), (A, R) ] = [ D, A ]
  | (key1, key2) `elem` [ (A, U), (D, L), (R, D) ] = [ L, A ]
  | (key1, key2) `elem` [ (U, A), (L, D), (D, R) ] = [ R, A ]
  | (key1, key2) `elem` [ (L, U), (D, A) ] = [ R, U, A ]
  | (key1, key2) == (U, L) = [ D, L, A ] -- to avoid gap 
  | (key1, key2) == (A, D) = [ L, D, A ]
  | (key1, key2) == (R, U) = [ L, U, A ]
  | (key1, key2) == (U, R) = [ D, R, A ]
  | (key1, key2) == (L, R) = [ R, R, A ]
  | (key1, key2) == (R, L) = [ L, L, A ]
  | (key1, key2) == (L, A) = [ R, R, U, A ]
  | (key1, key2) == (A, L) = [ D, L, L, A ] -- to avoid gap
  | key1 == key2 = [ A ]
  | otherwise = undefined -- Impossible

lift :: [Key] -> [Key]
lift keys = concat (mapPairs keySpan' (A : keys))

dirPad :: Array2 Key
dirPad = fromList [[undefined, U, A], [L, D, R]]

control :: [Key] -> [Key]
control = control' (0, 2)
  where
    control' i = \case
      [] -> []
      U:keys -> control' (add2 (-1, 0) i) keys
      D:keys -> control' (add2 (1, 0) i) keys
      L:keys -> control' (add2 (0, -1) i) keys
      R:keys -> control' (add2 (0, 1) i) keys
      A:keys -> dirPad ! i : control' i keys

controllingSequence :: String -> [Key]
controllingSequence nums = let
  keys = keySequence (map numPosition ('A' : nums))
  in lift (lift keys)

lenControllingSequence' :: Int -> String -> Int
lenControllingSequence' levels nums = 
  let arr = nest levels liftChunks (readChunks (controllingSequence nums))
  in sumMap (\c -> (sizes ! c) * (arr ! c)) [minBound..maxBound]

parse :: String -> [String]
parse = lines

part1 :: [String] -> Int
part1 = sumMap complexity
  where complexity str = read (init str) * length (controllingSequence str)

part2 :: [String] -> Int
part2 = sumMap complexity
  where complexity str = read (init str) * lenControllingSequence' 23 str