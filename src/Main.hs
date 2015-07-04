module Main where

import           Language.Forth

runProgram program = do
    contents <- readFile program
    return contents

main = putStrLn "Forth"
