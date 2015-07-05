module Language.AST where

-- Abstract Parse Tree
--
data AST =
    Def String String
  | Word String
  | Integer Int
  | Push AST
  | List [AST]
  deriving ( Show )

data Command =
    Add
  | Mult
  | Dup
  | Swap
  deriving ( Show )

data Prim =
    FInt    Int
  | FStr    String
  | FList   [Prim]
  | FNative Command

instance Show Prim where
    show (FInt x)   = (show x)
    show (FStr x)   = (show x)
    show (FList xs) = (show xs)
    show (FNative x) = (show x)
