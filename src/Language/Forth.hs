module Language.Forth () where

import           Control.Monad.State
import           Data.Map
import           Language.AST

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push y = state $ \xs -> ((), y:xs)

example = (push 10) >> (push 20) >> (push 30) >> pop

data Interpreter = Interpreter {
    stack :: [AST]
  , words :: Map String String
} deriving Show
