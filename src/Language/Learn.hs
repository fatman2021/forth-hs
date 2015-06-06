module Language.Learn where

import           Data.List (group, permutations, sort, subsequences)

-- Artificial Intelligence Combinatrics problem

operators :: [String]
operators =
  [ "+"
  , "DUP"
  , "SWAP"
  ]

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1]
                                  , x <- combinations (n-1) (drop (i+1) xs) ]

rep n xs =
  case n of
    1 -> xs
    2 -> xs ++ xs
    3 -> xs ++ xs ++ xs
    _ -> xs

-- Allow for using an operator multiple times
-- HOW DO YOU DO THIS ??
pimpedCombinations :: Int -> [a] -> [[a]]
pimpedCombinations n xs =
  case n of
    1 -> combinations n xs
    2 -> combinations n (xs ++ xs)
    3 -> combinations n (xs ++ xs ++ xs)

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

allPerms :: [a] -> [[a]]
allPerms xs = concat [f y xs | y <- [minBound..(length xs)]]
    where minBound = 1
          f n xs = concatMap permutations $ combinations n (rep n xs)

findSolutions :: Ord a => [a] -> [[a]]
findSolutions xs = removeDuplicates . allPerms $ xs

solve n = removeDuplicates . pimpedCombinations n
