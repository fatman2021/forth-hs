module Language.Scratch where

import           Control.Monad.State
import qualified Data.Map            as M

data Command =
    Push Int
  | Add
  | Mult
  | Dup
  | Swap
  | Dot
  | Subtract
  deriving ( Show )

type Program = [Command]
type Stack   = [Int]

popS :: State Stack Int
popS = state $ \(x:xs) -> (x,xs)

pushS :: Int -> State Stack ()
pushS y = state $ \xs -> ((), y:xs)

binaryOperation op = do
    x <- get
    y <- get
    put (x `op` y)

parseProgram :: String -> [Command]
parseProgram = (map asCommand) . words
    where
        asCommand "."    = Dot
        asCommand "+"    = Add
        asCommand "*"    = Mult
        asCommand "DUP"  = Dup
        asCommand "SWAP" = Swap
        asCommand x      = Push (read x)

push n stack = n : stack

add :: (MonadState b m, Num b) => t -> m b
add stack = do
  x <- get
  y <- get
  return $ x + y

runProgram :: Program -> Stack -> Stack
runProgram [] stack = stack
runProgram (x:xs) stack = runProgram xs (runCommand x stack)

runCommand :: Command -> Stack -> Stack
runCommand (Push n) stack = n : stack
runCommand Dup (x:xs)     = x:x:xs
runCommand Add (x:y:xs)   = x+y:xs
runCommand _ _ = error "empty stack"

execute :: String -> Stack -> Stack
execute program stack = runProgram parsed stack
  where parsed = parseProgram program

loop :: Stack -> IO String
loop stack = do
    putStrLn "Welcome to FORTH"
    putStrLn ">>>"
    putStrLn (show stack)
    code   <- getLine
    let result = execute code stack
    putStrLn . show $ result
    loop result
