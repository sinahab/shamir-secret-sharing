
module Shamir
  ( createShares,
    combineShares
  ) where

import System.Random
import Polynomial (constructPolynomial, evaluatePolynomial)

data Share = Share {x :: Int, y :: Int} deriving (Show, Eq)

-- @param Int secret the secret
-- @param Int k the threshold
-- @param Int n the total number of shares
-- @return [Share] a list of Share types.
createShares :: Int -> Int -> Int -> [Share]
createShares secret k n = [Share x (evaluatePolynomial x polynomial) | x <- [1..n]]
  where polynomial = constructPolynomial coefficients
        randomInts = take (k-1) $ randomRs (1,secret) (mkStdGen 11) :: [Int]
        coefficients = secret:randomInts

-- @param [Share] a list of Share types
-- @return Int the secret.
combineShares :: (Fractional a) => [Share] -> a
combineShares shares = sum [fromIntegral (y s1) * product [fromIntegral (x s2) / fromIntegral (x s2 - x s1) | s2 <- shares, s1 /= s2] | s1 <- shares]
-- alternative: 
-- combineShares shares = foldl (+) 0 [fromIntegral (y s1) * product [fromIntegral (x s2) / fromIntegral (x s2 - x s1) | s2 <- shares, s1 /= s2] | s1 <- shares]

