module Language.Forth where

import           Control.Monad.State
import qualified Data.Map            as M
import           Language.AST
import qualified Language.Parser     as P

type Dict = M.Map String String

data Interpreter = Interpreter {
    stack    :: [Prim]
  , wordList :: Dict
} deriving ( Show )

push :: a -> [a] -> [a]
push x stack = x : stack

pop (x:xs) = (x, xs)
pop []     = error "stack underflow"

type Stack = [Prim]

-- Functions

add :: Stack -> Stack
add ((FInt a):(FInt b):xs) =
  let result = FInt $ a + b in push result xs
add _ = error "Bad form"

-- TODO error here is around pushing ordering onto the stack!
dup :: Stack -> Stack
dup (x:xs) = (x:x:xs)
dup [] = []

--
eval :: AST -> Prim
eval (Word "+") = FNative Add
eval (Word "*") = FNative Mult
eval (Word "SWAP") = FNative Swap
eval (Word "DUP") = FNative Dup
eval (Integer x) = FInt x
eval (Word x) = FStr x
eval _ = error "Bad form"

-- Execution

run :: String -> [Prim]
run input = let ast = map eval $ P.parseAST input in foldl (flip push) [] ast

execute [] = []
execute stack@(x:xs) =
  case x of
    (FNative Add) -> execute $ add xs
    (FNative Dup) -> execute xs
    _             -> stack
