module CommutativeMonoidSpec (spec) where

import CommutativeMonoid
import Test.Hspec
import Test.QuickCheck

import Control.Monad (when)
import Data.Monoid

cols1 = [collectionFrom 5 1 5, collectionFrom 4 1 4, collectionFrom 5 1 5]
allOnes = replicate 10 (collectionFrom 1 1 1)

collectionFrom s c a = if a > 0 then Collection s c (Just a) else Collection s c Nothing

spec :: Spec
spec =
  describe "main" $ do
    it "averages a composition of 2 Collections" $ do
      (collectionFrom 1 1 1 <> collectionFrom 10 1 10) `shouldBe` collectionFrom 11 2 5.5
      (collectionFrom 11 2 5.5 <> collectionFrom 3 1 3) `shouldBe` collectionFrom 14 3 4.25

    it "averages a list of Collections" $ do
      mconcat cols1 `shouldBe` collectionFrom 14 3 4.75
      mconcat allOnes `shouldBe` collectionFrom 10 10 1

    it "counts positive values consistently" $ property $ \n ->
      count (mconcat $ replicate (abs n) (collectionFrom 1 1 1)) `shouldBe` abs n

    it "disallows negative values" pending
      -- (mconcat [Collection 1 1 (-1)]) `shouldThrow` anyException
