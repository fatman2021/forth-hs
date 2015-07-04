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
specialSymbols = "+-*/.!"

word :: Parser AST
word = many1 (letter <|> oneOf specialSymbols) >>= (\w -> return $ Word w)

-- | Experimental list form
list :: Parser AST
list = do
    char '['
    innerForms <- manyTill exprs (char ']')
    return $ List (foldr (++) [] innerForms)

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
expr = newWord <|> number <|> word <|> list

exprs :: Parser [AST]
exprs = expr `sepBy1` spaces

tryParser p input = case (parse (p `sepBy1` spaces) ">>" input) of
    Left e  -> error (show e)
    Right v -> v

parseAST :: String -> [AST]
parseAST input = case (parse exprs "FORTH" input) of
    Left _   -> []
    Right v  -> v
