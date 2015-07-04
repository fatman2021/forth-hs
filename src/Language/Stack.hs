module Language.Stack where

import           Control.Monad.State
import           Language.AST

type Stack = [Prim]

popS :: State Stack Prim
popS = state $ \(x:xs) -> (x,xs)

pushS :: Prim -> State Stack ()
pushS y = state $ \xs -> ((), y:xs)

dupS :: State Stack ()
dupS = do
    modify dup'
    return ()
    where
      dup' []     = fail "Empty stack"
      dup' (x:xs) = (x:x:xs)
