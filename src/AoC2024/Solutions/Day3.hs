{-# LANGUAGE LambdaCase #-}

module AoC2024.Solutions.Day3 (parse, part1, part2) where 

import Data.Maybe (fromMaybe, catMaybes)
import Data.List (foldl')
import Control.Applicative ((<|>), many)
import AoC2024.Parser (Parser, execParser, char, anyChar, string, natural)
import AoC2024.Utils (sumMap)

mulInstruction :: Parser (Int, Int)
mulInstruction = (,) <$ string "mul(" <*> natural <* char ',' <*> natural <* char ')'

data Instruction = Do | Dont | Mul (Int, Int)
  deriving (Show)

doInstruction :: Parser Instruction
doInstruction = Do <$ string "do()"

dontInstruction :: Parser Instruction
dontInstruction = Dont <$ string "don't()"

instruction :: Parser Instruction
instruction = doInstruction <|> dontInstruction <|> Mul <$> mulInstruction

interpret :: [Instruction] -> Int
interpret = fst . foldl' update (0, True)
  where
    update (n, enabled) = \case
      Do -> (n, True)
      Dont -> (n, False)
      Mul (a, b) -> (if enabled then a * b + n else n, enabled)

parse :: String -> String
parse = id

part1 :: String -> Int
part1 str = let
  result = execParser (many (Just <$> mulInstruction <|> Nothing <$ anyChar)) str
  pairs = catMaybes (fromMaybe [] result)
  in sumMap (uncurry (*)) pairs
  
part2 :: String -> Int
part2 str = let
  result = execParser (many (Just <$> instruction <|> Nothing <$ anyChar)) str
  instrs = catMaybes (fromMaybe [] result)
  in interpret instrs