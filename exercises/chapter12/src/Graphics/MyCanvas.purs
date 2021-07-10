module Graphics.MyCanvas (
    renderPath
  , Point
  , point
  , genRandomPoints
  , setGradientStrokeStyle
  , strokeAndFillPath
  , rotateAroundAPoint
  , degreeToRad
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Array (head, tail, (..))
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Effect (Effect, foreachE)
import Effect.Random (randomRange)
import Graphics.Canvas (CanvasGradient, Context2D, beginPath, closePath, fillPath, lineTo, moveTo, rotate, strokePath, translate)
import Math (pi, tau)

foreign import setGradientStrokeStyle :: Context2D -> CanvasGradient -> Effect Unit

type Point = { x :: Number, y :: Number }

handlePoints :: Context2D -> Point -> Array Point -> Effect Unit
handlePoints ctx begin rest = do
  moveTo ctx begin.x begin.y
  foreachE rest (\{x: x, y: y} -> lineTo ctx x y)

generatePath :: Context2D -> Array Point -> Effect Unit
generatePath ctx points = do
  beginPath ctx
  fromMaybe (pure unit) $ lift2 (handlePoints ctx) (head points) (tail points)
  closePath ctx

renderPath :: Context2D -> Array Point -> Effect Unit
renderPath ctx points = strokePath ctx (generatePath ctx points)

point :: Number -> Number -> Point
point x y = {x: x, y: y}

genRandomPoint :: Effect Point
genRandomPoint = do
 x <- randomRange 0.0 100.0
 y <- randomRange 0.0 251.0
 pure $ point x y

genRandomPoints :: Int -> Effect (Array Point)
genRandomPoints n = for (1..n) (\_ -> genRandomPoint)


strokeAndFillPath ::forall a. Context2D -> Effect a -> Effect a
strokeAndFillPath ctx path =  strokePath ctx $ fillPath ctx path

rotateAroundAPoint :: Context2D -> Number -> Number -> Effect Unit
rotateAroundAPoint ctx x y = do
  translate ctx {translateX: x, translateY: y}
  rotate ctx (degreeToRad 5.0)
  translate ctx {translateX: -x, translateY: -y}


degreeToRad :: Number -> Number
degreeToRad = (*) (pi / 180.0)