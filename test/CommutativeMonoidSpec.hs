module CommutativeMonoidSpec (spec) where

import CommutativeMonoid
import Prelude hiding (replicate, take)
import Test.Hspec
import Test.QuickCheck

import Control.Monad (when)
import Data.Monoid

cols1 = [collectionFrom 5 1 5, collectionFrom 4 1 4, collectionFrom 6 1 6]
allOnes = replicate 10 (collectionFrom 1 1 1)

collectionFrom s c a = if a > 0 then Collection s c (Just a) else Collection s c Nothing

take :: Integer -> [a] -> [a]
take n _      | n <= 0 =  []
take _ []              =  []
take n (x:xs)          =  x : take (n-1) xs

replicate :: Integer -> a -> [a]
replicate n x = take n (repeat x)

topsCollection1 = CollectionTops 1 (reverse [1..10])
spec :: Spec
spec = do
  describe "collection" $ do
    it "averages a composition of 2 Collections" $ do
      (collectionFrom 1 1 1 <> collectionFrom 10 1 10) `shouldBe` collectionFrom 11 2 5.5
      (collectionFrom 11 2 5.5 <> collectionFrom 4 1 4) `shouldBe` collectionFrom 15 3 5.0

    it "averages a list of Collections" $ do
      mconcat cols1 `shouldBe` collectionFrom 15 3 5.0
      mconcat allOnes `shouldBe` collectionFrom 10 10 1

    it "counts positive values consistently" $ property $ \n ->
      count (mconcat $ replicate (abs n) (collectionFrom 1 1 1)) `shouldBe` abs n

    it "disallows negative values" pending
      -- (mconcat [Collection 1 1 (-1)]) `shouldThrow` anyException

  describe "collectionTops" $ do
    it "collects the top10" $
      topsCollection1 <> mempty `shouldBe` topsCollection1

    it "collects the top10" $ do
      let cs = topsCollection1 <> CollectionTops 3000 negativeInfinities
      head (tops cs) `shouldBe` 3000
      (1 `elem` tops cs) `shouldBe` False

