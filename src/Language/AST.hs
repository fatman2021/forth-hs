module Language.AST
  (AST(..)) where

data AST =
    Def String String
  | Word String
  | Number Int
  deriving ( Show, Eq, Ord )
