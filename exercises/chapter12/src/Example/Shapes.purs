module Example.Shapes where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (addColorStop, arc, closePath, createLinearGradient, fillPath, getCanvasElementById, getContext2D, lineTo, moveTo, rect, setFillStyle, setLineWidth, strokeRect)
import Graphics.MyCanvas (setGradientStrokeStyle)
import Math as Math
import Partial.Unsafe (unsafePartial)

translate
  :: forall r
   . Number
  -> Number
  -> { x :: Number, y :: Number | r }
  -> { x :: Number, y :: Number | r }
translate dx dy shape = shape
  { x = shape.x + dx
  , y = shape.y + dy
  }

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  -- Square
  setFillStyle ctx "#00F"

  fillPath ctx $ rect ctx $ translate (-200.0) (-200.0)
    { x: 250.0
    , y: 250.0
    , width: 100.0
    , height: 100.0
    }

  -- Triangle
  setFillStyle ctx "#0F0"

  fillPath ctx $ arc ctx $ translate 200.0 200.0
    { x: 300.0
    , y: 300.0
    , radius: 50.0
    , start: 0.0
    , end: Math.tau * 2.0 / 3.0
    }

  -- partial circle
  setFillStyle ctx "#F00"

  fillPath ctx $ do
    moveTo ctx 300.0 260.0
    lineTo ctx 260.0 340.0
    lineTo ctx 340.0 340.0
    closePath ctx

  -- rectangle with gradient

  gradient <- createLinearGradient ctx {x0: 0.0, y0: 0.0, x1: 170.0, y1: 0.0}

  addColorStop gradient 0.0 "magenta"
  addColorStop gradient 0.5 "blue"
  addColorStop gradient 1.0 "red"

  setLineWidth ctx 5.0
  setGradientStrokeStyle ctx gradient
  strokeRect ctx $ translate 0.0 450.0 {x: 20.0, y: 20.0, width: 150.0, height: 100.0}