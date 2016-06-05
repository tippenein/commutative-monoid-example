module CommutativeMonoid where
import Control.Monad
import Data.List
import Data.Monoid

data Collection = Collection
  { theSum :: Integer
  , count  :: Int
  , avg    :: Maybe Double
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
      average = pure $ realToFrac (avg1 + avg2) / 2
  mconcat = foldl' mappend mempty
