module Language.AST
  (AST(..)) where

data AST =
    Def String String
  | Word String
  | Number Int
  | Push AST
  | List [AST]
  deriving ( Show, Eq, Ord )
