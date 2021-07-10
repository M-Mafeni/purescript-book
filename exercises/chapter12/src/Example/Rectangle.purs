module Example.Rectangle where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Example.Shapes (translate)
import Graphics.Canvas (arc, fillPath, getCanvasElementById, getContext2D, moveTo, rect, setFillStyle)
import Graphics.MyCanvas (degreeToRad)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle ctx "#00F"

  fillPath ctx $ do
    let rectangle = { x: 250.0
                    , y: 250.0
                    , width: 100.0
                    , height: 100.0
                    }
    rect ctx rectangle
    rect ctx $ translate 120.0 0.0 rectangle
    moveTo ctx 50.0 50.0
    arc ctx {x: 50.0, y: 50.0, radius: 100.0, start: 0.0, end: degreeToRad 60.0 }

