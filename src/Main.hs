module Main where

import           Control.Monad.State

-- Haskell Stack DataStructure
--

type Stack a = [a]

create :: Stack a
create = []

push :: a -> Stack a -> Stack a
push = (:)

pop :: Stack a -> (a, Stack a)
pop []     = error "Stack empty"
pop (x:xs) = (x,xs)

peek :: Stack a -> Maybe a
peek []     = Nothing
peek (x:xs) = Just x
