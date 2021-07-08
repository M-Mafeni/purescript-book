module Test.MySolutions where

import Prelude

import Control.Monad.Except (ExceptT, lift, throwError)
import Control.Monad.Reader (Reader, ReaderT, ask, local, runReader, runReaderT)
import Control.Monad.State (State, StateT, execState, get, modify, put)
import Control.Monad.Writer (Writer, WriterT, execWriterT, runWriter, tell)
import Data.Foldable (traverse_)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), joinWith, stripPrefix)
import Data.String.CodeUnits (drop, take, toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple)

testParens :: String -> Boolean
testParens s = balanced == 0 where
  balanced = execState (parensState (toCharArray s)) 0

processChar :: Char -> Int -> Int
processChar '(' x = if x >= 0 then x + 1 else x
processChar ')' x = x - 1
processChar _ x = x

parensState :: Array Char -> State Int Unit
parensState = traverse_ $ \n -> modify (processChar n)

type Level = Int

type Doc = Reader Level String

line :: String -> Doc
line sentence = do
  level <- ask
  pure $ ("  " `power` level) <> sentence

indent :: Doc -> Doc
indent = local (\n -> n + 1)

-- Array (Reader Level String) -> Reader Level (Array String)
cat :: Array Doc -> Doc
cat xs = joinWith "\n" <$> sequence xs

render :: Doc -> String
render doc = runReader doc 0

sumArrayWriter :: Array Int -> Writer (Additive Int) Unit
sumArrayWriter = (traverse_ tell <<< map Additive)

collatz :: Int -> Tuple Int (Array Int)
collatz = (runWriter <<< collatzWriter 0)

collatzWriter :: Int -> Int -> Writer (Array Int) Int
collatzWriter x 1 = do
    tell [1]
    pure x
collatzWriter i n = do
    tell [n]
    let x = if (n `mod` 2) == 0 then n / 2 else 3 * n + 1
    collatzWriter (i+1) x

safeDivide :: Number -> Number -> ExceptT String Identity Number
safeDivide _ 0.0 = throwError "Tried to divide by 0"
safeDivide x y = pure $ x / y

type Errors = Array String

type Log = Array String

type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

string :: String -> Parser String
string prefix = do
  s <- get
  lift $ tell ["The state is " <> s]
  case stripPrefix (Pattern prefix) s of
    Nothing -> lift $ lift $ throwError ["Could not parse"]
    Just suffix -> do
      put suffix
      pure prefix

type Document = ReaderT Int (WriterT (Array String) Identity)

line' :: String -> Document Unit
line' sentence = do
  level <- ask
  lift $ tell $ [("  " `power` level) <> sentence]
  pure unit

indent' :: forall a. Document a -> Document a
indent' = local (\n -> n + 1)


render' :: Document Unit -> String
render' document =  joinWith "\n" $ unwrap $ execWriterT $ runReaderT document 0