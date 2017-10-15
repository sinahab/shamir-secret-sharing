
module Polynomial
  ( Term (..),
    Polynomial (..),
    constructPolynomial,
    evaluatePolynomial
  ) where

data Polynomial = Polynomial {order :: Int, terms :: [Term]} deriving (Show, Eq)
data Term = Term {degree :: Int, coeff :: Integer} deriving (Show, Eq)

-- |Constructs a polynomial using the provided values as coefficients,
-- with the coefficient's index taken as the order of term it applies to. 
-- @param [Integer]: a list of coefficients.
-- @return Polynomial: a polynomial.
constructPolynomial :: [Integer] -> Polynomial
constructPolynomial coefficients = Polynomial order terms
  where terms = [(Term degree (coefficients !! degree)) | degree <- [0..order]]
        order = (length coefficients) - 1

-- |Evaluate a polynomial at point x.
-- @param Integer: the point at which to evaluate the polynomial.
-- @param Polynomial: the polynomial to evaluate.
-- @return Integer: the value of the polynomial at the desired point.
evaluatePolynomial :: Integer -> Polynomial -> Integer
evaluatePolynomial x polynomial = evaluateSum x $ terms polynomial
  where evaluateSum x [] = 0
        evaluateSum x (t:ts) = (coeff t) * x ^ (degree t) + evaluateSum x ts
