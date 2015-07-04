module Language.Scratch where

import           Control.Monad.State
import qualified Data.Map            as M
import Data.List
import qualified Language.Learn as Learn
import Control.Exception

-- | Base language forms (int string list and function)
data Prim =
    FInt    Int
  | FStr    String
  | FList   [Prim]
  | FNative Command

data Command =
    Add
  | Mult
  | Dup
  | Swap
  | Dot
  deriving ( Show )

type Program = [Command]
type Stack   = [Int]

popS :: State Stack Int
popS = state $ \(x:xs) -> (x,xs)

pushS :: Int -> State Stack ()
pushS y = state $ \xs -> ((), y:xs)

dup :: State Stack ()
dup = do
    modify dup'
    return ()
    where
      dup' []     = fail "Empty stack"
      dup' (x:xs) = (x:x:xs)

binaryOp :: (Int -> Int -> Int) -> State Stack ()
binaryOp op = do
    modify (binOp' op)
    return ()
      where
        binOp' op (x:y:xs) = let z = x `op` y in z : xs
        binOp' op _        = error "Invalid function arity"

pop2 :: State [Int] (Maybe Int)
pop2 = do
    (x:xs) <- get
    put xs
    return $ Just x

parseProgram :: String -> [Command]
parseProgram = (map asCommand) . words
    where
        asCommand "."    = Dot
        asCommand "+"    = Add
        asCommand "*"    = Mult
        asCommand "DUP"  = Dup
        asCommand "SWAP" = Swap

push :: a -> [a] -> [a]
push n stack = n : stack

add :: (MonadState b m, Num b) => t -> m b
add stack = do
  x <- get
  y <- get
  return $ x + y

runCommand :: Command -> Stack -> Either String Stack
runCommand Dup (x:xs)     = Right $ x : x : xs
runCommand Add (x:y:xs)   = Right $ x + y : xs
runCommand Mult (x:y:xs)  = Right $ x * y : xs
runCommand Swap (x:y:xs)  = Right $ y : x : xs
runCommand _ _ = Left "empty stack"

-- runProgram :: Program -> Either String Stack -> Either String Stack
-- runProgram [] stack = stack
-- runProgram (x:xs) stack = runProgram xs (runCommand x stack)
--    where commandResult = runCommand x stack

--execute :: String -> Stack -> Either String Stack
--execute program stack = runProgram parsed stack
--  where parsed = parseProgram program
