module WordVal where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)

newtype WordVal = WordVal String

derive instance newtypeWordVal :: Newtype WordVal _

derive instance eqWordVal :: Eq WordVal


instance arbWordVal :: Arbitrary WordVal where
  arbitrary = map WordVal arbitrary

instance coarbWordVal :: Coarbitrary WordVal where
  coarbitrary (WordVal x) = coarbitrary x

toString :: WordVal -> String
toString = unwrap

fromString :: String -> WordVal
fromString = wrap

