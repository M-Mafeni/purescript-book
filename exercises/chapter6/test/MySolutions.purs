module Test.MySolutions where

import Prelude

import Data.Array (length, nub, nubByEq, nubEq, (:))
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (fromJust)
import Data.Monoid (power)
import Data.Newtype (class Newtype, over2)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)

-- Note to reader: Add your solutions to this file

newtype Point
  = Point
  { x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
    show (Point {x: x, y: y}) = joinWith "" ["(", show x, ", ", show y, ")"]

newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

derive instance equalComplex :: Eq Complex
instance newtypeComplex :: Newtype Complex { real :: Number, imaginary :: Number}
instance showComplex :: Show Complex where
    show (Complex {real: real, imaginary: imaginary}) = show real <> imaginaryPart where
        imaginaryPart
            | imaginary == 0.0 = ""
            | imaginary < 0.0 = show imaginary <> "i"
            | otherwise = "+" <> show imaginary <> "i"

instance semiringComplex :: Semiring Complex where
    zero = Complex {real: 0.0, imaginary: 0.0}
    one = Complex {real: 1.0, imaginary: 0.0}
    add = over2 Complex (\{real: r1, imaginary: i1} {real: r2, imaginary: i2} -> {real: r1 + r2, imaginary: i1 + i2})
    mul = over2 Complex (\{real: r1, imaginary: i1} {real: r2, imaginary: i2} -> {real: r1 * r2 - i1 * i2, imaginary: r1 * i2 + i1 * r2})

derive newtype instance ringComplex :: Ring Complex
-- instance ringComplex :: Ring Complex where
--     sub = over2 Complex (\{real: r1, imaginary: i1} {real: r2, imaginary: i2} -> {real: r1 - r2, imaginary: i1 - i2})

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance genericShow :: Generic Shape _
instance showShape :: Show Shape where
    show = genericShow

derive instance equalPoint :: Eq Point
derive instance equalShape :: Eq Shape

derive instance ordPoint :: Ord Point
derive instance ordShape :: Ord Shape

data NonEmpty a = NonEmpty a (Array a)
derive instance equalNonEmpty :: Eq a => Eq (NonEmpty a)
instance showNonEmpty :: Show a => Show (NonEmpty a) where
    show (NonEmpty x xs) = "NonEmpty " <> show x <> " " <> show xs 

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> (y:ys))

instance functorNonEmpty :: Functor NonEmpty where
    map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)

data Extended a = Infinite | Finite a
derive instance eqExtended :: Eq a => Eq (Extended a)
instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare _ Infinite = LT
  compare Infinite _ = GT
  compare (Finite x) (Finite y) = compare x y

instance foldableNonEmpty :: Foldable NonEmpty where
    foldr f k (NonEmpty x xs) = foldr f k (x:xs)
    foldl f k (NonEmpty x xs) = foldl f k (x:xs)
    foldMap f (NonEmpty x xs) = foldMap f (x:xs)

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
    foldr f k (OneMore x xs) =  f x (foldr f k xs)
    foldl f k (OneMore x xs) =  foldl f (f k x) xs
    foldMap f (OneMore x xs) =  f x <> foldMap f xs

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum = fromJust <<< maximum

class Monoid m <= Action m a where
  act :: m -> a -> a
 
newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance actionMultiplyInt :: Action Multiply Int where
  act (Multiply x) y = x * y

instance actionMultiplyString :: Action Multiply String where
  act (Multiply n) word = power word n

instance actionArray :: Action m a => Action m (Array a) where
--   act :: m -> Array a -> Array a
  act m = map (act m)

newtype Self m = Self m
derive newtype instance showSelf :: Show a => Show (Self a)
derive newtype instance eqSelf :: Eq a => Eq (Self a)

derive newtype instance showMultiply :: Show Multiply
derive newtype instance eqMultiply :: Eq Multiply

instance actionSelf :: Monoid m => Action m (Self m) where
  act x (Self y) = Self $ x `append` y

arrayHasDuplicates :: forall (a :: Type). Hashable a => Array a -> Boolean

arrayHasDuplicates xs = length (nubByEq (\x y -> hash x == hash y && x == y) xs) /= length xs

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashableHour :: Hashable Hour where
  hash (Hour n) = hash $ mod n 12