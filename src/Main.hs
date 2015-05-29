module Main where

import           Control.Arrow
import           Control.Monad.State

-- Haskell Stack DataStructure

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push y = state $ \xs -> ((), y:xs)

peek' [] = Nothing
peek' (x:xs) = Just x

binOp op = do
    x <- pop
    y <- pop
    push $ x `op` y

plusOp = binOp (+)
minusOp = binOp (-)

example = (push 10) >> (push 20) >> (push 30) >> pop

x = runState example []
