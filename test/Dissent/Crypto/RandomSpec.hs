{-# LANGUAGE OverloadedStrings #-}

module Dissent.Crypto.RandomSpec where

import Data.List (sort)
import qualified Dissent.Crypto.Random as R
import qualified Dissent.Util as U

import Test.Hspec

spec :: Spec
spec = do
  describe "our extract function" $ do
    it "extracts the first item properly" $
      R.extract 0 [1,2,3] `shouldBe` (1, [2,3])
    it "extracts the last item properly" $
      R.extract 2 [1,2,3] `shouldBe` (3, [1,2])
    it "extracts the middle item properly" $
      R.extract 1 [1,2,3] `shouldBe` (2, [1,3])

  describe "our shuffleWithDistribution function" $ do
    it "should reverse the list when providing an appropriate random sequence" $
      R.shuffleWithDistribution [1,2,3] [2,1,0] `shouldBe` [3,2,1]
    it "should keep the list in tact when only taking the first element" $
      R.shuffleWithDistribution [1,2,3] [0,0,0] `shouldBe` [1,2,3]

  describe "our generateDistribution function" $ do
    it "should generate a sequence with the appropriate length" $ do
      distribution <- R.generateDistribution 5
      length (distribution) `shouldBe` 5

    it "should generate appropriate numbers" $ do
      distribution <- R.generateDistribution 10

      let extractAndValidate offset number = do
            let (i, _) = R.extract offset distribution
            i < number

      extractAndValidate 0 10 `shouldBe` True
      extractAndValidate 1 9  `shouldBe` True
      extractAndValidate 2 8  `shouldBe` True
      extractAndValidate 3 7  `shouldBe` True
      extractAndValidate 4 6  `shouldBe` True
      extractAndValidate 5 5  `shouldBe` True
      extractAndValidate 6 4  `shouldBe` True
      extractAndValidate 7 3  `shouldBe` True
      extractAndValidate 8 2  `shouldBe` True
      extractAndValidate 9 1  `shouldBe` True

  describe "our shuffle function" $ do
    it "should not return the same list" $ do
      list     <- U.randomList 52
      shuffled <- R.shuffle list

      list `shouldSatisfy` (/= shuffled)

    it "contain the same numbers" $ do
      list     <- U.randomList 52
      shuffled <- R.shuffle list

      -- Quick 'n' dirty check whether the lists contain the same numbers
      sort (list) `shouldBe` sort (shuffled)
