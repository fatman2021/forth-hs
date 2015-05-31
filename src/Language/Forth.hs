module Language.Forth () where

import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Language.AST

type Stack = [Int]

popS :: State Stack Int
popS = state $ \(x:xs) -> (x,xs)

pushS :: Int -> State Stack ()
pushS y = state $ \xs -> ((), y:xs)

example = (pushS 10) >> (pushS 20) >> (pushS 30) >> popS

----

type ForthStack = [Int]

type Dict = Map String String

data Interpreter = Interpreter {
    stack    :: ForthStack
  , wordList :: Dict
} deriving ( Show )

defaultInterpreter :: Interpreter
defaultInterpreter = Interpreter [] M.empty

addWord :: Interpreter -> String -> String -> Interpreter
addWord (Interpreter stack wordList) name value =
  Interpreter stack (M.insert name value wordList)

doCommand (Interpreter stack wordList) (Word "DUP") =
    case stack of
      []     -> error "Stack underflow"
      (x:xs) -> Interpreter (x:x:xs) wordList
