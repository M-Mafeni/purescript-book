module Test.Main where

import Prelude

import Data.Array (all, reverse, sort, sortBy)
import Data.Array.NonEmpty (toUnfoldable1)
import Data.Foldable (foldMap, foldr)
import Data.Function (on)
import Data.List (List(..), fromFoldable, toUnfoldable)
import Data.Maybe (Maybe(..), fromJust)
import Data.Maybe.First (First(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Merge (merge, mergePoly, mergeWith)
import Partial.Unsafe (unsafePartial)
import Sorted (sorted)
import Test.QuickCheck (Result(..), mkSeed, quickCheck, quickCheckPure, (<?>))
import Tree (Tree, member, insert, toArray, anywhere)
import WordVal (WordVal(..)) as W
import WordVal (WordVal)

isSorted :: forall a. (Ord a) => Array a -> Boolean
isSorted = go <<< fromFoldable
  where
  go (Cons x1 t@(Cons x2 _)) = x1 <= x2 && go t
  go _ = true

ints :: Array Int -> Array Int
ints = identity

intToBool :: (Int -> Boolean) -> Int -> Boolean
intToBool = identity

treeOfInt :: Tree Int -> Tree Int
treeOfInt = identity

bools :: Array Boolean -> Array Boolean
bools = identity

wordVal :: WordVal -> WordVal
wordVal = identity


main :: Effect Unit
main = do
  -- Tests for module 'Merge'

  quickCheck \xs ys ->
    let
      result = merge (sort xs) (sort ys)
      expected = sort $ xs <> ys
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected

  quickCheck \xs ->
    let
      result = merge (sort xs) []
      expected = sort xs
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected
    
  quickCheck \xs ys ->
    eq (merge (sorted xs) (sorted ys)) (sort $ sorted xs <> sorted ys)

  quickCheck \xs ys ->
    eq (ints $ mergePoly (sorted xs) (sorted ys)) (sort $ sorted xs <> sorted ys)

  quickCheck \xs ys ->
    eq (bools $ mergePoly (sorted xs) (sorted ys)) (sort $ sorted xs <> sorted ys)

  quickCheck \xs ys f ->
    let
      result = map f $ mergeWith (intToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)
      expected = map f $ sortBy (compare `on` f) $ xs <> ys
    in
      eq result expected

  -- mergeWith associativity
  quickCheck \xs ys zs f ->
    let
      sortedXs = sortBy (compare `on` f) xs
      sortedYs = sortBy (compare `on` f) ys
      sortedZs = sortBy (compare `on` f) zs
      r1 = map f $ mergeWith f (mergeWith (intToBool f) sortedXs sortedYs) sortedZs
      r2 = map f $ mergeWith (intToBool f) sortedXs (mergeWith f sortedYs sortedZs)
    in
      eq r1 r2
  -- reverse check
  quickCheck \xs ->
    eq (ints xs) (( reverse <<< reverse <<< ints) xs) 

  quickCheck \xs ->
    eq (bools xs) (( reverse <<< reverse <<< bools) xs)

  -- Sanity Test for wordval
  quickCheck \v -> eq (wordVal v) (wordVal v)

  -- Tests for module 'Tree'

  quickCheck \t a -> member a $ insert a $ treeOfInt t
  quickCheck \t xs -> isSorted $ toArray $ foldr insert t $ ints xs

  quickCheck \f g t ->
    anywhere (\s -> f s || g s) t ==
      anywhere f (treeOfInt t) || anywhere g t

mergeResults = quickCheckPure (mkSeed 12345) 10 $ \xs ys zs f ->
    let
      sortedXs = sortBy (compare `on` f) xs
      sortedYs = sortBy (compare `on` f) ys
      sortedZs = sortBy (compare `on` f) zs
      r1 = map f $ mergeWith f (mergeWith (intToBool f) sortedXs sortedYs) sortedZs
      r2 = map f $ mergeWith (intToBool f) sortedXs (mergeWith f sortedYs sortedZs)
    in
      eq r1 r2

squashResultBool :: List Result -> Boolean
squashResultBool = all (\x -> show x == "Success") <<< toUnfoldable

squashResults :: List Result -> Result
squashResults =  unsafePartial fromJust <<< unwrap <<< foldMap (First <<< Just)