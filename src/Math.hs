module Math
  ( extendedEuclid
  , modMultInverse
  ) where

-- |Calculates the GCD and coeffcients of Bézout's identity for a and b s.t.: ax + by = gcd(a,b).
-- Inspired by: https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
-- @param Integer: a the first number
-- @param Integer: b the second number
-- @return (Integer, Integer, Integer): (gcd, x, y) from Bézout's identity.
extendedEuclid :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclid 0 b = (b, 0, 1)
extendedEuclid a b = (gcd, y - (b `div` a) * x, x)
  where (gcd, x, y) = extendedEuclid (b `mod` a) a

-- |Calculates the multiplicative inverse of a s.t.: a * b = 1 mod p
-- WARNING: only works if a and p are co-prime.
-- @param Integer: the number a 
-- @param Integer: the field p
-- @return Integer: b i.e. the multiplicative inverse of a modulo p
modMultInverse :: Integer -> Integer -> Integer
modMultInverse a p = get2nd $ extendedEuclid a p
  where get2nd (_,x,_) = x

