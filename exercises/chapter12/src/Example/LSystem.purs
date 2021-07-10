module Example.LSystem where

import Prelude

import Data.Array (catMaybes, concatMap, foldM)
import Data.Maybe (Maybe(..))
import Data.String (toUpper)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Graphics.Canvas (closePath, fillPath, getCanvasElementById, getContext2D, lineTo, moveTo, setFillStyle, setShadowBlur, setShadowColor, setShadowOffsetX, setShadowOffsetY)
import Math as Math
import Partial.Unsafe (unsafePartial)

lsystem :: forall a m s
         . Monad m
         => Array a
         -> (a -> Array a)
         -> (s -> a -> m s)
         -> Int
         -> s
         -> m s
lsystem init prod interpret n state = foldM interpret state finalSentence where
  finalSentence :: Array a
  finalSentence = buildFinalSentence n init where
    buildFinalSentence :: Int -> Array a -> Array a
    buildFinalSentence 0 acc = acc
    buildFinalSentence i acc = buildFinalSentence (i-1) (concatMap prod acc)


data Letter = L | R | F

type Sentence = Array Letter

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

initial :: Sentence
initial = [F, R, R, F, R, R, F, R, R]

productions :: Letter -> Sentence
productions L = [L]
productions R = [R]
productions F = [F, L, F, R, R, F, L, F]

data LetterExtended = ExtL | ExtR | ExtF | ExtM
toLetterExtended :: String -> Array LetterExtended
toLetterExtended = (catMaybes <<< map fromChar <<< toCharArray <<< toUpper) where
  fromChar 'L' = Just ExtL
  fromChar 'R' = Just ExtR
  fromChar 'F' = Just ExtF
  fromChar 'M' = Just ExtM
  fromChar  _  = Nothing

productionsExt :: LetterExtended -> Array LetterExtended
productionsExt ExtL = [ExtL]
productionsExt ExtR = [ExtR]
productionsExt ExtF = toLetterExtended "FLMLFRMRFRMRFLMLF"
productionsExt ExtM = toLetterExtended "MRFRMLFLMLFLMRFRM"

initialExtended :: Array LetterExtended
initialExtended = toLetterExtended "FRRFRRFRR"

initialState :: State
initialState = { x: 120.0, y: 200.0, theta: 0.0 }

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  setFillStyle ctx "#000"
  moveTo ctx initialState.x initialState.y
  let
    interpret :: State -> Letter -> Effect State
    interpret state L = pure $ state { theta = state.theta - Math.tau / 6.0 }
    interpret state R = pure $ state { theta = state.theta + Math.tau / 6.0 }
    interpret state F = do
      let x = state.x + Math.cos state.theta * 1.5
          y = state.y + Math.sin state.theta * 1.5
      lineTo ctx state.x state.y
      lineTo ctx x y
      pure { x, y, theta: state.theta }

    interpretExt :: State -> LetterExtended -> Effect State
    interpretExt state ExtL = pure $ state { theta = state.theta - Math.tau / 6.0 }
    interpretExt state ExtR = pure $ state { theta = state.theta + Math.tau / 6.0 }
    interpretExt state _ = do
      let x = state.x + Math.cos state.theta * 1.5
          y = state.y + Math.sin state.theta * 1.5
      lineTo ctx state.x state.y
      lineTo ctx x y
      pure { x, y, theta: state.theta }

  setShadowOffsetX ctx 25.0
  setShadowOffsetY ctx 25.0
  setShadowBlur ctx 15.0
  setShadowColor ctx "grey"
  void $ fillPath ctx $ lsystem initialExtended productionsExt interpretExt 3 initialState
  closePath ctx
