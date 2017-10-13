module Polynomial
  ( constructPolynomial,
    evaluatePolynomial
  ) where

data Coeff = Coeff {degree :: Int, value :: Int} deriving (Show)

-- @param [Int] list of integer coefficients. The index of each determines which term it's a coefficient of.
-- @return [Coeff] list of Coeff defining the polynomial
constructPolynomial :: [Int] -> [Coeff]
constructPolynomial coefficients = [(Coeff d (coefficients !! d)) | d <- [0..(degree - 1)]]
  where degree = length coefficients

-- Evaluate polynomial at value x.
-- @param Int x where to evaluate the polynomial.
-- @param [Coeff] list of coefficients representing the polynomial.
-- @return Int the value of the polynomial at the desired point.
evaluatePolynomial :: Int -> [Coeff] -> Int
evaluatePolynomial x [] = 0
evaluatePolynomial x (c:cs) = (value c) * x ^ (degree c) + evaluatePolynomial x cs

