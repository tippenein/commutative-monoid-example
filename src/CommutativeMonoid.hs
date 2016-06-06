module CommutativeMonoid where
import Control.Monad
import Data.List
import Data.Monoid

data Collection = Collection
  { theSum :: Integer
  , count  :: Integer
  , avg    :: Maybe Double
  } deriving (Show, Eq)

data CollectionTops = CollectionTops
  { aSum :: Integer
  , tops :: [Integer]
  } deriving (Show, Eq)

instance Monoid Collection where
  mempty = Collection 0 0 Nothing
  (Collection sum1 count1 Nothing) `mappend` (Collection sum2 count2 whatever) =
    Collection (sum1 + sum2) (count1 + count2) whatever
  (Collection sum1 count1 a) `mappend` (Collection sum2 count2 Nothing) =
    Collection (sum1 + sum2) (count1 + count2) a
  (Collection sum1 count1 (Just avg1)) `mappend` (Collection sum2 count2 (Just avg2)) =
    Collection (sum1 + sum2) (count1 + count2) average
    where
      average = Just $
        ((avg1 * fromInteger count1) + (avg2 * fromInteger count2)) /
          (fromInteger count1 + fromInteger count2)
  mconcat = foldl' mappend mempty

negativeInfinities = replicate 10 (-1)
revSort = sortBy (flip compare)
instance Monoid CollectionTops where
  mempty = CollectionTops 0 negativeInfinities
  CollectionTops s1 tops1 `mappend` CollectionTops s2 tops2 =
    CollectionTops (s1 + s2) (take 10 $ revSort ([s1] ++ [s2] ++ tops1 ++ tops2))
  mconcat = foldl' mappend mempty

