module Test.MySolutions where

import Prelude

import Control.MonadZero (guard)
import Data.Array (all, filter, foldl, foldr, head, length, nubByEq, null, tail, (..), (:))
import Data.Int.Bits (xor)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Data.Path (Path(..), filename, isDirectory, ls, size)
import Partial.Unsafe (unsafePartial)
import Test.Examples (allFiles, factors)


infix 8 filter as <$?>

-- Note to reader: Add your solutions to this file
isEven :: Int -> Boolean
isEven x = if x == 0 then true else 
            if x == 1 then false else
            if x < 0 then isEven (x+2)
            else isEven (x-2)

countEven :: Array Int -> Int
countEven xs = if null xs then 0 else
                if (isEven $ unsafePartial $ fromJust (head xs)) then 1 + countEven (fromMaybe [] (tail xs)) else
                    countEven (fromMaybe [] (tail xs))

squared :: Array Number -> Array Number
squared = map (\x -> x*x)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (\x -> x >= 0.0)

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite xs =  (\x -> x >= 0.0) <$?> xs

isPrime :: Int -> Boolean
isPrime x = (x /= 1) && ((\xs -> length xs == 1 ) $ factors x)

cartesianProduct :: forall (a :: Type) (b:: Type). Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
    x <- xs
    y <- ys
    pure [x,y]

triples :: Int -> Array (Array Int)
triples n = do
    k <- 1..n
    j <- 1..k
    i <- 1..j
    guard $ i*i + j*j == k*k
    pure [i,j,k]

allPrime :: Array Int -> Boolean
allPrime = all isPrime

-- factorize :: Int -> Array Int
-- factorize n = if n == 1 then [] else do
--     factorsList <- factors n
--     let x =  unsafePartial fromJust $ head factorsList
--     let y = (unsafePartial fromJust <<< head <<< unsafePartial fromJust <<< tail) factorsList
--     guard $ isPrime x
--     let rest = factorize y
--     guard $ allPrime rest
--     pure x

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

{-
 F [T,F,T]
 
 Arrays where xs contains an odd number of  false values
-}

fibTailRec :: Int -> Int
fibTailRec n = fib' n 1 0 where
  fib' i curr prev = if i == 0 
        then prev
        else fib' (i-1) (curr+prev) curr
reverse :: forall (a :: Type). Array a -> Array a
reverse = foldl f k where 
    k = []
    f reversedArray y =  y:reversedArray

onlyFiles :: Path -> Array Path
onlyFiles =  filter (not isDirectory) <<< allFiles

whereIs :: Path -> String -> Maybe Path
whereIs path name = fromMaybe Nothing $ head $ whereIs' path name

whereIs' :: Path -> String -> Array (Maybe Path)
whereIs' path name = do
    file <- ls path
    if isDirectory file then 
        whereIs' file name
    else if (filename file == filename path <> name)
        then pure (Just path)
        else []

getLargest' :: Path -> Maybe Path
getLargest' = f Nothing where
    f :: Maybe Path -> Path -> Maybe Path
    f largestFile path = if not isDirectory path 
            then case largestFile of
                Nothing -> Just path
                Just file -> if (size path > size file) then Just path else Just file
            else if (length  (ls path) == 0)
                then Nothing
                else foldr g Nothing $ map (getLargest') (ls path) where
                    g (Just x) (Just acc) = if (size x > size acc) then (Just x) else Just acc
                    g x Nothing = x
                    g Nothing y = y

getSmallest' :: Path -> Maybe Path
getSmallest' = f Nothing where
    f :: Maybe Path -> Path -> Maybe Path
    f largestFile path = if not isDirectory path 
            then case largestFile of
                Nothing -> Just path
                Just file -> if (size path < size file) then Just path else Just file
            else if (length  (ls path) == 0)
                then Nothing
                else foldr g Nothing $ map (getSmallest') (ls path) where
                    g (Just x) (Just acc) = if (size x < size acc) then (Just x) else Just acc
                    g x Nothing = x
                    g Nothing y = y

getLargest :: Path -> Maybe Path
getLargest = getLargest' 

getSmallest :: Path ->  Maybe Path
getSmallest = getSmallest'

largestSmallest :: Path -> Array Path
largestSmallest path =  (nubByEq (\f1 f2 -> filename f1 == filename f2) <<< map (unsafePartial fromJust) <<< filter isJust) [getLargest path, getSmallest path]