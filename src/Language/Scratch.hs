module Language.Scratch where

data Command =
    Push Int
  | Add
  | Mult
  | Dup
  | Dot
  | Subtract
  deriving ( Show )

type Program = [Command]
type Stack   = [Int]

parseProgram :: String -> [Command]
parseProgram = map asCommand . words
    where
        asCommand "."   = Dot
        asCommand "+"   = Add
        asCommand "*"   = Mult
        asCommand "DUP" = Dup
        asCommand x     = Push (read x)

runProgram :: Program -> Stack -> Stack
runProgram [] stack = stack
runProgram (x:xs) stack = runProgram xs (runCommand x stack)

runCommand :: Command -> Stack -> Stack
runCommand (Push n) stack = n : stack
runCommand Dup (x:xs) = x:x:xs
runCommand Add (x:y:xs) = x+y:xs

forth program = (\p -> runProgram p []) . parseProgram
