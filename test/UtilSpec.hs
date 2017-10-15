module UtilSpec (main, spec) where

import Test.Hspec
import Control.Exception (evaluate)

import Util (combinations)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "combinations" $ do
    it "Returns all possible combinations of n elements." $ do
      let myList = [1,2,3,4]
      
      combinations 1 myList `shouldBe` [[1], [2], [3], [4]]      
      combinations 2 myList `shouldBe` [[1,2], [1,3], [1,4], [2,3], [2,4], [3,4]]
      combinations 3 myList `shouldBe` [[1,2,3], [1,2,4], [1,3,4], [2,3,4]]
      combinations 4 myList `shouldBe` [[1,2,3,4]]
