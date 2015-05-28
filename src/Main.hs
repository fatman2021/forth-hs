module Main where

import           Control.Arrow
import           Control.Monad.State

-- Haskell Stack DataStructure

type Stack a = [a]

create :: Stack a
create = []

push :: a -> Stack a -> Stack a
push = (:)

pop :: Stack a -> Stack a
pop []     = []
pop (x:xs) = xs

peek :: Stack a -> Maybe a
peek []     = Nothing
peek (x:xs) = Just x

runBinaryOp stack op =
  let a = pop stack
      b = pop stack in
  push (a `op` b)

data AST =
    Command String
  | Atom String
  deriving ( Show, Eq )

data VirtualMachine = VirtualMachine {
  initialStack :: Stack AST
}
