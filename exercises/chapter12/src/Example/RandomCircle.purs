module Example.RandomCircle where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Random (random)
import Graphics.Canvas (Context2D, arc, fillPath, getCanvasElementById, getContext2D, rect, setFillStyle, setStrokeStyle, withContext)
import Graphics.MyCanvas (strokeAndFillPath)
import Math (tau)
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (toEventTarget)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)


renderRandomCircle :: Context2D -> Effect Unit
renderRandomCircle ctx = do

  setFillStyle ctx "#FFF"

  fillPath ctx $ rect ctx
    { x: 0.0
    , y: 0.0
    , width: 600.0
    , height: 600.0
    }
  
  setFillStyle ctx "#F00"
  setStrokeStyle ctx "#000"
  withContext ctx do
    x <- random
    y <- random
    r <- random

    let path = arc ctx
          { x     : x * 600.0
          , y     : y * 600.0
          , radius: r * 50.0
          , start : 0.0
          , end   : tau
          }

    strokeAndFillPath ctx path
    


main :: Effect Unit
main = do
  possCanvas <- getCanvasElementById "canvas"
  case possCanvas of
    Just canvas -> do
                    ctx <- getContext2D canvas
                    renderRandomCircle ctx
                    doc <- map (toParentNode <<< toDocument) (document =<< window)
                    possNode <- querySelector (QuerySelector "#canvas") doc
                    case possNode of
                      Just node -> do 
                        clickListener <- eventListener $ \_ -> renderRandomCircle ctx
                        addEventListener (EventType "click") clickListener true (toEventTarget node)
                      Nothing -> log "Could not find node"

    Nothing -> log "Could Not find element called canvas"