module MathSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Math (extendedEuclid, modMultInverse)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "extendedEuclid" $ do
    it "Calculates the GCD and coefficients of Bézout's identity for the inputs." $ do
      extendedEuclid 6 17 `shouldBe` (1,3,-1)
      extendedEuclid 7 17 `shouldBe` (1,5,-2)
      extendedEuclid 9 7  `shouldBe` (1,-3,4)

  describe "modMultInverse" $ do
    it "Calculates the multiplicative inverse for the number" $ do
      ((2 * modMultInverse 2 7) `mod` 7) `shouldBe` 1
      ((6 * modMultInverse 6 11) `mod` 11) `shouldBe` 1
      ((4 * modMultInverse 4 1439) `mod` 1439) `shouldBe` 1
