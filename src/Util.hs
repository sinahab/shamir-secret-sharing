module Util
  ( combinations
  ) where

-- |Returns all possible combinations of n elements as a new list.
-- Taken from: https://stackoverflow.com/questions/8779765/permutations-of-a-list-haskell
-- @param Int: number of elements in new lists.
-- @param [a]: a list of elements.
-- @return [[a]]: a list of lists, each of the length desired.
combinations :: Int -> [a] -> [[a]]
combinations k xs = combinations' (length xs) k xs
  where combinations' n k' l@(y:ys)
          | k' == 0   = [[]]
          | k' >= n   = [l]
          | null l    = []
          | otherwise = map (y :) (combinations' (n - 1) (k' - 1) ys) ++ combinations' (n - 1) k' ys 

