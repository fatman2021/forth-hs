module Language.Core
  ( add
  , dup
  , swap
  ) where

import           Language.AST

add :: [Prim] -> [Prim]
add ((FInt x) : (FInt y) : xs) = (FInt $ x + y) : xs
add _ = fail "Invalid arguments"

dup :: [a] -> [a]
dup (x:xs) = (x:x:xs)
dup [] = []

swap :: [a] -> [a]
swap (x:y:xs) = (y:x:xs)
swap x        = x
