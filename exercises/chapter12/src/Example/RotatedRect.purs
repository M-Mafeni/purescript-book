module Example.RotatedRect where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Ref as Ref
import Graphics.Canvas (Context2D, fillPath, getCanvasElementById, getContext2D, rect, rotate, scale, setFillStyle, translate, withContext)
import Graphics.MyCanvas (rotateAroundAPoint)
import Math as Math
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (toEventTarget)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

render :: Context2D -> Effect Unit
render ctx  = do
  setFillStyle ctx "#FFF"
  fillPath ctx $ rect ctx
    { x: 0.0
    , y: 0.0
    , width: 600.0
    , height: 600.0
    }
  rotateAroundAPoint ctx 300.0 300.0
  renderRect ctx

renderRect :: Context2D -> Effect Unit
renderRect ctx = do
  setFillStyle ctx "#0F0"
  fillPath ctx $ rect ctx
    { x: 150.0
    , y: 150.0
    , width: 200.0
    , height: 200.0
    }
  
main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  renderRect ctx
  doc <- map (toParentNode <<< toDocument) (document =<< window)
  Just node <- querySelector (QuerySelector "#canvas") doc

  clickListener <- eventListener $ \_ -> do
    logShow "Mouse clicked"
    render ctx

  addEventListener (EventType "click") clickListener true (toEventTarget node)
