{-# LANGUAGE LambdaCase #-}

module AoC2024 where

import Data.List (sort, foldl')
import Data.Char (isDigit)
import Data.Bifunctor (first)
import Control.Applicative (Alternative(empty, (<|>), many))
import Control.Monad (MonadPlus)
import Data.Maybe (fromMaybe, catMaybes)
import Debug.Trace

-- Answers for future reference:
-- Day 1: 2285373, 21142653
-- Day 2: 686, 717
-- Day 3: 174336360

alternate :: [a] -> ([a], [a])
alternate = \case
  [] -> ([], [])
  [x] -> ([x], [])
  x:y:zs -> let (xs, ys) = alternate zs in (x:xs, y:ys)

count :: (a -> Bool) -> [a] -> Int
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



parse1 :: IO ([Int], [Int])
parse1 = alternate . map read . words <$> readFile "input/day1.txt"

day1Part1 :: ([Int], [Int]) -> Int
day1Part1 (xs, ys) =
  sum (map abs (zipWith (-) (sort xs) (sort ys)))

-- Inefficient but runs fast enough
day1Part2 :: ([Int], [Int]) -> Int
day1Part2 (xs, ys) =
  sum (map (\x -> x * count (== x) ys) xs)



safe :: [Int] -> Bool
safe report = 
  let ds = differences report in 
  all ((>= -3) &&& (<= -1)) ds || all ((>= 1) &&& (<= 3)) ds

parse2 :: IO [[Int]]
parse2 = map (map read . words) . lines <$> readFile "input/day2.txt"

day2Part1 :: [[Int]] -> Int
day2Part1 = count safe

day2Part2 :: [[Int]] -> Int
day2Part2 = count (any safe . faces)



-- I could have used a library for this, of course,
-- but I thought that implementing it myself would be a fun exercise
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

execParser :: Parser a -> String -> Maybe a
execParser p s = fmap fst (runParser p s)

instance Functor Parser where
  fmap f p = Parser $ \s -> fmap (first f) (runParser p s)

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  pf <*> px = 
    Parser $ \s -> do
      (f, s') <- runParser pf s
      (x, s'') <- runParser px s'
      Just (f x, s'')

instance Monad Parser where
  p >>= f =
    Parser $ \s -> do
      (x, s') <- runParser p s
      runParser (f x) s'

instance Alternative Parser where
  empty = Parser (const Nothing)
  px <|> py = Parser $ \s -> runParser px s <|> runParser py s

instance MonadPlus Parser where

char :: Char -> Parser ()
char c = Parser $ \case
  x:xs | x == c -> Just ((), xs)
  _ -> Nothing

anyChar :: Parser ()
anyChar = Parser $ \case
  [] -> Nothing
  _:xs -> Just ((), xs)

string :: String -> Parser ()
string = mapM_ char

natural :: Parser Int
natural = Parser $ \s ->
  let (n, s') = span isDigit s in
  if null n then Nothing else Just (read n, s')

mulInstruction :: Parser (Int, Int)
mulInstruction = (,) <$ string "mul(" <*> natural <* char ',' <*> natural <* char ')'

parse3 :: IO String
parse3 = readFile "input/day3.txt"

day3Part1 :: String -> Int
day3Part1 str = let
  result = execParser (many ((Just <$> mulInstruction) <|> (Nothing <$ anyChar))) str
  pairs = catMaybes (fromMaybe [] result)
  in sum (map (uncurry (*)) pairs)
  
day3Part2 :: String -> Int
day3Part2 = undefined