{-# LANGUAGE LambdaCase #-}
module AoC2024.Parser where

import Data.Maybe (fromMaybe)
import Control.Applicative (Alternative, empty, (<|>), many)
import Control.Monad (MonadPlus)
import Data.Bifunctor (first)
import Data.Char (isDigit)

-- I could have used a library for this, of course,
-- but I thought that implementing it myself would be a fun exercise

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

execParser :: Parser a -> String -> Maybe a
execParser p s = fmap fst (runParser p s)

partialExecParser :: Parser a -> String -> a
partialExecParser p s = fromMaybe (error "Parsing failed") (execParser p s)

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
  -- Backtracks
  px <|> py = Parser $ \s -> runParser px s <|> runParser py s

instance MonadPlus Parser where

eof :: Parser ()
eof = Parser $ \case
  [] -> Just ((), [])
  _:_ -> Nothing

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

parseWhile :: (Char -> Bool) -> Parser String
parseWhile p = Parser (Just . span p)

natural :: Parser Int
natural = Parser $ \s ->
  let (n, s') = span isDigit s in
  if null n then Nothing else Just (read n, s')

integer :: Parser Int
integer = do
  minus <- (char '-' >> return True) <|> return False
  n <- natural
  return (if minus then -n else n)

separatedBy :: Parser () -> Parser a -> Parser [a]
separatedBy sep p = (:) <$> p <*> many (sep >> p)

endedBy :: Parser () -> Parser a -> Parser [a]
endedBy sep p = many (p <* sep)