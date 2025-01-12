module OneTwoThree where

import Prelude

import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)
import Test.QuickCheck.Gen (oneOf)

data OneTwoThree a = One a | Two a a | Three a a a

instance arbOneTwoThree :: Arbitrary a => Arbitrary (OneTwoThree a) where
  arbitrary = oneOf $ NonEmptyArray [
                                         One <$> arbitrary
                                        , Two <$> arbitrary <*> arbitrary
                                        , Three <$> arbitrary <*> arbitrary <*> arbitrary
                                    ]

instance coarbOneTwoThree :: Coarbitrary a => Coarbitrary (OneTwoThree a) where
  coarbitrary (One x) = coarbitrary x
  coarbitrary (Two x y) = coarbitrary x <<< coarbitrary y
  coarbitrary (Three x y z) = coarbitrary x <<<
                                coarbitrary y <<<
                                coarbitrary z

                            
