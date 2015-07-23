module Language.Forth where

import           Control.Monad.State
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Language.AST
import qualified Language.Core       as Core
import qualified Language.Parser     as P
import           Language.Stack

type Dict = M.Map String String

data Interpreter = Interpreter {
    stack    :: [Prim]
  , wordList :: Dict
} deriving ( Show )

internal :: M.Map String Prim
internal = M.fromList [ ("+",    FNative Add)
                      , ("*",    FNative Mult)
                      , ("SWAP", FNative Swap)
                      , ("DUP",  FNative Dup)
                      ]

-- AST transformation
--
eval :: AST -> Prim
eval (Word x) = fromMaybe (FStr x) (M.lookup x internal)
eval (Integer x) = FInt x
eval _ = error "Bad form"

-- Take a program input string and turn it into a runnable stack to be executed
--
parseProgram :: String -> Stack
parseProgram input = map eval $ P.parseAST input

-- Execution (proceeds recursively to ensure forms are fully evaluated)
--
execute [] = []
execute stack@(x:xs) =
  case x of
    FNative Add   -> execute $ Core.add  (execute xs)
    FNative Dup   -> execute $ Core.dup  (execute xs)
    FNative Swap  -> execute $ Core.swap (execute xs)
    _             -> stack

-- Run a forth program
--
-- λ> forth "1 2 + DUP DUP"
-- [3, 3, 3]
--
-- forth input = execute . foldPush $ parseProgram input
