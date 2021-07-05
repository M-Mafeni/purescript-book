module Test.MySolutions where

import Prelude

import Control.Monad.ST (for, run)
import Control.Monad.ST.Internal (modify, new, read)
import Data.Array (head, nub, sort, tail)
import Data.Foldable (foldM)
import Data.Int (pow, toNumber)
import Data.List (List(..), snoc)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Exception (error, throwException)

-- Note to reader: Add your solutions to this file
third :: forall a. Array a -> Maybe a
third xs = do
  r1 <- tail xs
  r2 <- tail r1
  head r2

possibleSums :: Array Int -> Array Int
possibleSums = sort <<< nub <<< foldM f 0 where
    f :: Int -> Int -> Array Int 
    f x y = [x, y, x + y]


filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM f  = foldM g Nil where
    -- g :: List a -> a ->  m (List a)
    g xs x = do
        val <- f x
        if val then pure (snoc xs x) else pure xs

exceptionDivide :: Int -> Int -> Effect Int
exceptionDivide _ 0 = throwException $ error "div zero"
exceptionDivide x y = pure $ x / y

estimatePi :: Int -> Number
estimatePi = estimatePiHelper 1.0 where
    estimatePiHelper acc 1 = 4.0 * acc
    estimatePiHelper acc n = estimatePiHelper (acc + nthTerm) (n-1) where
        nthTerm = (toNumber $ (-1) `pow` (n+1)) / (toNumber (2*n -1))


estimatePiState :: Int -> Number
estimatePiState n = run do
    ref <- new 0.0
    for 1 (n + 1) \k ->
        modify (\acc -> acc + ((toNumber $ (-1) `pow` (k+1)) / (toNumber (2*k -1))) ) ref
    final <- read ref
    pure $ final * 4.0

fibTailRec :: Int -> Int
fibTailRec 0 = 0
fibTailRec 1 = 1
fibTailRec n = fib' n 2 0 1
  where
  fib' :: Int -> Int -> Int -> Int -> Int
  fib' limit count n1 n2 =
    if limit == count then
      n1 + n2
    else
      fib' limit (count + 1) n2 (n1 + n2)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = run do
  ref <- new {curr: 1, prev: 0}
  for 2 x (\_ ->
    modify (\{curr: c, prev: p} -> {curr: c + p, prev: c}) ref
  )
  final <- read ref
  pure $ final.curr + final.prev