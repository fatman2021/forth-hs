{-# LANGUAGE OverloadedStrings #-}

module Language.Forth
  ( exprs
  , parseAST
  ) where

import           Control.Applicative ((*>), (<*))
import           Language.AST
import           Text.Parsec
import           Text.Parsec.String

number :: Parser AST
number = do
  n <- many1 digit
  return $ Number (read n)

specialSymbols :: [Char]
specialSymbols = "+-*/."

word :: Parser AST
word = do
  wrd <- many1 (letter <|> oneOf specialSymbols)
  return $ Word wrd

-- Forth words
--
-- : inc (x -- x) 1 + ;
--
newWord :: Parser AST
newWord = do
    char ':'
    space
    name <- many1 letter
    space
    body <- manyTill anyChar (char ';')
    return $ (Def name body)

expr :: Parser AST
expr = newWord <|> number <|> word

exprs :: Parser [AST]
exprs = expr `sepBy1` spaces

parseAST :: String -> [AST]
parseAST input = case (parse exprs "FORTH" input) of
    Left _   -> []
    Right v  -> v
