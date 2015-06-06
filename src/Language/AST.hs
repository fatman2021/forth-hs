module Language.AST
  (AST(..)) where

data AST =
    Def String String
  | Word String
  | Number Int
  | Push AST
  deriving ( Show, Eq, Ord )
