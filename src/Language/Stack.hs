module Language.Stack where

import           Control.Monad.State
import           Language.AST

type Stack = [Prim]

-- Stateful stack
--

popS :: State Stack Prim
popS = state $ \(x:xs) -> (x,xs)

pushS :: Prim -> State Stack ()
pushS x = state $ \xs -> ((), x:xs)

-- Non stateful stack
--

push :: a -> [a] -> [a]
push = (:)

foldPush :: [a] -> [a]
foldPush = foldl (flip push) []

pop :: [a] -> (a, [a])
pop (x:xs) = (x, xs)
pop _      = error "Stack underflow"
