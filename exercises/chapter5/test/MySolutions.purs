module Test.MySolutions where

import Prelude

import ChapterExamples (Amp(..), Person, Volt(..))
import Data.Maybe (Maybe(..))
import Data.Picture (Point, Shape(..), origin)
import Math (pi)

factorial :: Int -> Int
factorial x
    | x == 0 = 1
    | otherwise = x * factorial (x-1) 

binomial :: Int -> Int -> Int
binomial x k 
    | k == 0 = 1
    | x == 0 = 0
    | k > x = 0
    | otherwise = (factorial x) / ((factorial k) * factorial (x - k))

pascal :: Int -> Int -> Int
pascal n k
    | k == 0 = 1
    | n == 0 = 0
    | k > n = 0
    | otherwise = binomial (n-1) k + binomial (n-1) (k-1)

sameCity :: Person -> Person -> Boolean
sameCity {address: {city: x}} {address: {city: y}} = x == y

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton default _ = default

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

scalePoint :: Point -> Number -> Point
scalePoint {x: x, y: y} scaleFactor = {x: x * scaleFactor, y: y * scaleFactor} 

shiftPoint :: Point -> Point -> Point
shiftPoint {x: x1, y: y1} {x: x2, y: y2} = {x: x1 + x2, y: y1 + y2}

getMidPoint :: Point -> Point -> Point
getMidPoint {x: x1, y: y1} {x: x2, y: y2} = {x: (x1+x2)/2.0, y:(y1+y2)/2.0}
doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle _ r) = Circle origin (r*2.0)
doubleScaleAndCenter (Rectangle _ w h) = Rectangle origin (w*2.0) (h*2.0)
doubleScaleAndCenter (Text _ name) = Text origin name
doubleScaleAndCenter line = (shiftLine <<< scaleLine) line where
  scaleLine (Line p1 p2) = Line (p1 `scalePoint` 2.0) (p2 `scalePoint` 2.0)
  scaleLine x = x
  shiftLine (Line p1 p2) = Line (shiftPoint p1 negMidPoint) (shiftPoint p2 negMidPoint) where
    negMidPoint = flip scalePoint (-1.0) $ getMidPoint p1 p2
  shiftLine x = x
    

shapeText :: Shape -> Maybe String
shapeText (Text _ name) = Just name
shapeText _ = Nothing

newtype Watt = Watt Number
calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp x) (Volt y) = Watt (x*y)

area :: Shape -> Number
area (Circle _ r) = pi * r * r
area (Rectangle _ w h) = w * h
area _ = 0.0