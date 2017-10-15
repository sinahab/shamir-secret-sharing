module PolynomialSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Polynomial

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "constructPolynomial" $ do
    it "Constructs a polynomial with the provided coefficients" $ do
      -- f(x) = 1 + x^2
      let expectedPolynomial = Polynomial 2 [Term 0 1, Term 1 0, Term 2 1]
      let actualPolynomial = constructPolynomial [1, 0, 1]
      actualPolynomial `shouldBe` expectedPolynomial

  describe "evaluatePolynomial" $ do
    it "Evaluates a polynomial at a given point" $ do
      -- f(x) = 2 + 3 x^2 + 4 x^3 + x^5
      let polynomial = constructPolynomial [2, 0, 3, 4, 0, 1]
      evaluatePolynomial 3 polynomial `shouldBe` 380
      evaluatePolynomial 5 polynomial `shouldBe` 3702
