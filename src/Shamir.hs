
module Shamir
  ( Share(..),
    createShares,
    combineShares
  ) where

import System.Random
import Polynomial (constructPolynomial, evaluatePolynomial)
import Math (modMultInverse)

data Share = Share {x :: Integer, y :: Integer} deriving (Show, Eq)

-- this is the prime finite-field size,
-- referred to when creating and combining shares.
prime = 5749 :: Integer

-- |Divides the secret into k-out-of-n shares.
-- Uses the Shamir secret sharing algorithm.
-- @param Integer: the secret
-- @param Integer: k, the threshold required to recover the secret.
-- @param Integer: n, the total number of shares
-- @return IO [Share]: an IO list of shares.
createShares :: Integer -> Integer -> Integer -> IO [Share]
createShares secret k n
  | (k <= 0) || (n <= 0) = error $ "k and n must be positive integers."
  | k > n = error $ "k must be less-than-or-equal-to n."
  | secret < 0 = error $ "The secret cannot be negative."
  | secret > prime = error $ "The secret must be smaller than field size, i.e. " ++ show prime ++ "."
  | otherwise = do
    generator <- getStdGen
    let polynomial = constructPolynomial coefficients
        coefficients = secret:randomInts
        randomInts = take (fromInteger $ k-1) $ randomRs (1, prime-1) generator :: [Integer]
    return $ [Share x (evaluatePolynomial x polynomial `mod` prime) | x <- [1..n]]

-- |Combines shares to recover a secret.
-- Uses Lagrange interpolation, solving for f(0).
-- It will only return the correct secret if the required number of shares are provided.
-- @param [Share]: shares to combine.
-- @return Integer: the secret.
combineShares :: [Share] -> Integer
combineShares shares = 
  let total = foldl (+) 0 terms
      terms = [(y s1) * product [(x s2) * modMultInverse ((x s2 - x s1) `mod` prime) prime | s2 <- shares, s1 /= s2] | s1 <- shares]
  in total `mod` prime
