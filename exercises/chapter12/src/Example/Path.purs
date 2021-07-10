module Example.Path where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.MyCanvas (genRandomPoints, renderPath)
  
main :: Effect Unit
main = do
  possCanvas <- getCanvasElementById "canvas"
  case possCanvas of
    Just canvas -> do
                    ctx <- getContext2D canvas
                    path <- genRandomPoints 50
                    renderPath ctx path
    Nothing -> log "Could Not find element called canvas"
