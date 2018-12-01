{-# LANGUAGE TupleSections #-}
module Text.Parser
  (
    Parser
  , runParser
  , one
  , filterP
  , string
  )
where

import qualified Data.Text as T
import Control.Applicative
import Control.Arrow(first,second)
import Data.Char

newtype Parser a = Parser { runParser :: T.Text -> Maybe (a, T.Text) }

instance Functor Parser where
  fmap f (Parser parser) = Parser $ \s ->
    fmap (first f) $ parser s

instance Applicative Parser where
  pure x = Parser $ \s -> pure (x, s)
  Parser p <*> Parser q = Parser $ \s -> do
    (f, s') <- p s
    (x, r) <- q s'
    return (f x, r)

instance Monad Parser where
  Parser p >>= f = Parser $ \s -> do
    (x, s') <- p s
    runParser (f x) s'

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p <|> Parser q = Parser $ \s ->
    p s <|> q s

one :: Parser Char
one = Parser $ \s -> if T.null s
  then Nothing
  else Just (T.head s, T.tail s)

filterP :: (a -> Bool) -> Parser a -> Parser a
filterP p parser = do
  x <- parser
  if p x then return x else empty

string :: T.Text -> Parser T.Text
string target = Parser $ fmap (target,) . T.stripPrefix target

whitespace :: Parser T.Text
whitespace = Parser $ \s ->
  Just $ T.span isSpace s
