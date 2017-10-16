module ShamirSpec (main, spec) where

import Test.Hspec

import Shamir (createShares, combineShares, Share (..))
import Util (combinations)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "createShares" $ do
    it "Creates n shares" $ do
      shares <- createShares 1234 3 6
      length shares `shouldBe` 6 
      map (\s -> x s) shares `shouldBe` [1,2,3,4,5,6]

  describe "combineShares" $ do
    it "Combines shares to create the secret" $ do
      let secret = 1234
      shares <- createShares secret 3 6 

      -- functions used to test whether the secret was recovered.
      let assertNoSecret availableShares = (combineShares availableShares) `shouldNotBe` secret
      let assertSecret availableShares = (combineShares availableShares) `shouldBe` secret

      -- combinations of lt threshold shares do not recreate the secret.
      mapM_ assertNoSecret $ combinations 1 shares
      mapM_ assertNoSecret $ combinations 2 shares
      -- combinations of gte threshold shares do recreate the secret.
      mapM_ assertSecret $ combinations 3 shares
      mapM_ assertSecret $ combinations 4 shares
      mapM_ assertSecret $ combinations 5 shares
      mapM_ assertSecret $ combinations 6 shares
