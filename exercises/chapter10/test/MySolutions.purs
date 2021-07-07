module Test.MySolutions where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), caseJsonArray, decodeJson, encodeJson, jsonParser, printJsonDecodeError)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array (length, (!!))
import Data.Either (Either(..), note)
import Data.Function.Uncurried (Fn3)
import Data.Generic.Rep (class Generic)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Pair (Pair(..))
import Data.Set as S
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Test.Examples (Quadratic, Undefined, Complex, isUndefined)

-- Note to reader: Add your solutions to this file
foreign import volumeFn :: Fn3 Number Number Number Number
foreign import volumeArrow :: Number -> Number -> Number -> Number
foreign import cumulativeSumsComplex :: Array Complex -> Array Complex
foreign import quadraticRootsImpl :: (forall a. a -> a -> Pair a) -> Quadratic -> Pair Complex
foreign import unsafeFromUndefined :: forall a. Undefined a -> a
foreign import valuesOfMapImpl :: Json -> Json
foreign import quadraticRootsSetImpl :: Quadratic -> Json
foreign import quadRoots :: Json -> Json 


quadraticRoots :: Quadratic -> Pair Complex
quadraticRoots = quadraticRootsImpl Pair

toMaybe :: forall a. Undefined a -> Maybe a
toMaybe x
    | isUndefined x = Nothing
    | otherwise = Just $ unsafeFromUndefined x

valuesOfMap :: M.Map String Int -> Either JsonDecodeError (S.Set Int)
valuesOfMap = encodeJson >>> valuesOfMapImpl >>> decodeJson 

valuesOfMapGeneric :: forall k v. Ord k => Ord v => EncodeJson k => EncodeJson v => DecodeJson v => M.Map k v -> Either JsonDecodeError (S.Set v)
valuesOfMapGeneric = encodeJson >>> valuesOfMapImpl >>> decodeJson 

quadraticRootsSet :: Quadratic -> Either JsonDecodeError (S.Set Complex)
quadraticRootsSet = quadraticRootsSetImpl >>> decodeJson

newtype ComplexPair = ComplexPair (Pair Complex)

fromComplexPair :: ComplexPair -> Pair Complex
fromComplexPair (ComplexPair p) = p
getPairFromArray :: Array Complex -> Maybe (ComplexPair) 
getPairFromArray xs
    | (length xs) /= 2 = Nothing
    | otherwise = ComplexPair <$> (lift2 Pair) (xs !! 0) (xs !! 1)
instance decodeComplexPair :: DecodeJson ComplexPair where
    decodeJson json = do
        xs <- decodeJson json
        note (TypeMismatch "Expected Array") (getPairFromArray xs)

quadraticRootsSafe :: Quadratic -> Either JsonDecodeError (Pair Complex)
quadraticRootsSafe = encodeJson >>> quadRoots >>> decodeJson >>> map fromComplexPair

-- Array JSON -> Array (Array Int)
-- JSON -> Either JsonError (Array Int)
-- Array Json -> Either JsonError (Array (Array Int))
convertJsonErrorToString :: forall a. Either JsonDecodeError a -> Either String a
convertJsonErrorToString (Left x) = Left (printJsonDecodeError x)
convertJsonErrorToString (Right v) = Right v

to2DArrayInt :: Array Json -> Either String (Array (Array Int))
to2DArrayInt = convertJsonErrorToString <$> traverse decodeJson
parseAndDecodeArray2D :: String -> Either String (Array (Array Int))
parseAndDecodeArray2D x = do
    json <- jsonParser x
    caseJsonArray (Left "Wrong Type: Expected Array") (to2DArrayInt) json

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)

derive instance genericTree :: Generic (Tree a) _

instance encodeJsonTree :: EncodeJson a => EncodeJson (Tree a) where
    encodeJson x = genericEncodeJson x

instance decodeJsonTree :: DecodeJson a => DecodeJson (Tree a) where
    decodeJson x = genericDecodeJson x

instance showTree :: Show a => Show (Tree a) where
    show x = genericShow x

derive instance eqTree :: (Eq a) => Eq (Tree a)


data IntOrString
  = IntOrString_Int Int
  | IntOrString_String String

derive instance eqIntOrString :: Eq IntOrString
instance showIntOrString :: Show IntOrString where
    show (IntOrString_Int x) = "IntOrString_Int " <> show x
    show (IntOrString_String s) = "IntOrString_String " <> show s
instance encodeJsonIntOrString :: EncodeJson IntOrString where
    encodeJson (IntOrString_Int x) = encodeJson x
    encodeJson (IntOrString_String x) = encodeJson x

fromJsonInt :: Json -> Either JsonDecodeError Int
fromJsonInt = decodeJson

fromJsonString :: Json -> Either JsonDecodeError String
fromJsonString = decodeJson

instance decodeJsonIntOrString :: DecodeJson IntOrString where
    decodeJson json = (IntOrString_Int <$> fromJsonInt json) <|> (IntOrString_String <$> fromJsonString json)
        