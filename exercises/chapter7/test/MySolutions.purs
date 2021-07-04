module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.AddressBook (Address, PhoneNumber, address)
import Data.AddressBook.Validation (Errors, matches, nonEmpty, validateAddress, validatePhoneNumber, validatePhoneNumbers)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Validation.Semigroup (V)

-- Note to reader: Add your solutions to this file

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe = lift2 (+)

subMaybe :: Maybe Int -> Maybe Int -> Maybe Int
subMaybe = lift2 (-)

mulMaybe :: Maybe Int -> Maybe Int -> Maybe Int
mulMaybe = lift2 (*)

divMaybe :: Maybe Int -> Maybe Int -> Maybe Int
divMaybe = lift2 (/)


addApply :: forall (f :: Type -> Type). (Apply f) => f Int -> f Int -> f Int
addApply = lift2 (+)

subApply :: forall (f :: Type -> Type). (Apply f) => f Int -> f Int -> f Int
subApply = lift2 (-)

mulApply :: forall (f :: Type -> Type). (Apply f) => f Int -> f Int -> f Int
mulApply = lift2 (*)

divApply :: forall (f :: Type -> Type). (Apply f) => f Int -> f Int -> f Int
divApply = lift2 (/)

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just f) = map Just f

stateRegex :: Regex
stateRegex = unsafeRegex "^[a-zA-Z]{2}$" noFlags

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "\\S" noFlags

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a = address <$>
    matches "Street"  nonEmptyRegex a.street
    <*> matches "City" nonEmptyRegex a.city
    <*> matches "State" stateRegex a.state

data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance eqTree :: Eq a => Eq (Tree a)
derive instance genericTree :: Generic (Tree a) _
instance showTree :: Show a => Show (Tree a) where
    show x = genericShow x

derive instance functorTree :: Functor Tree
instance foldTree :: Foldable Tree where
    -- foldr :: (a -> b -> b) -> b -> (Tree a) -> b
    foldr _ k Leaf = k
    foldr f k (Branch t1 x t2) = foldr f (f x (foldr f k t2)) t1
    foldl _ k Leaf = k
    foldl f k (Branch t1 x t2) = foldl f (f (foldl f k t1) x) t2
    -- foldMap :: (a -> m) -> Tree a -> m
    foldMap f tree = foldr append mempty (map f tree)
 
instance traversableTree :: Traversable Tree where
    -- traverse :: (a -> m b) -> Tree a -> m (Tree b)
    traverse _ Leaf = pure Leaf
    -- map f tree :: Tree (m b)
    -- foldMap g tree :: m b
    traverse f (Branch t1 x t2) = Branch <$> (traverse f t1)
                                    <*> f x 
                                    <*> (traverse f t2)
    -- sequence :: Tree (m a) -> m (Tree a)
    sequence Leaf = pure Leaf
    sequence (Branch t1 x t2) = Branch <$> (sequence t1) 
                                <*> x <*> (sequence t2)

traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder _ Leaf = pure Leaf
traversePreOrder f (Branch t1 x t2) = ado
  root <- f x
  left <- traversePreOrder f t1
  right <- traversePreOrder f t2
  in Branch left root right

traversePostOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder _ Leaf = pure Leaf
traversePostOrder f (Branch t1 x t2) = ado
  left <- traversePostOrder f t1
  right <- traversePostOrder f t2
  root <- f x
  in Branch left root right

type PersonOptional
  = { firstName :: String
    , lastName :: String
    , homeAddress :: Maybe Address
    , phones :: Array PhoneNumber
    }


personOptional :: String -> String -> Maybe Address -> Array PhoneNumber -> PersonOptional
personOptional firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }

validatePersonOptionalAddress :: PersonOptional -> V Errors PersonOptional
validatePersonOptionalAddress p = personOptional 
        <$> matches "First Name" nonEmptyRegex p.firstName
        <*> matches "Last Name" nonEmptyRegex p.lastName
        <*> traverse validateAddress p.homeAddress
        <*> validatePhoneNumbers "Phone Numbers" p.phones
sequenceUsingTraverse :: forall t a m. Traversable t => Applicative m => t (m a) -> m (t a)
sequenceUsingTraverse = traverse identity

traverseUsingSequence :: forall a b m t. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverseUsingSequence f = sequence <<< map f