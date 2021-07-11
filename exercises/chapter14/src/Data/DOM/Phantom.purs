module Data.DOM.Phantom
  ( Element
  , Attribute
  , Content
  , AttributeKey
  , class IsValue
  , toValue
  , EmptyAttribute

  , ElementSize
  , percent
  , pixel
  , a
  , p
  , img

  , disabled
  , checked

  , href
  , _class
  , src
  , width
  , height

  , attribute, (:=)
  , text
  , elem
  , emptyAttribute, (:~)

  , render
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (joinWith)

newtype Element = Element
  { name         :: String
  , attribs      :: Array Attribute
  , content      :: Maybe (Array Content)
  }

data Content
  = TextContent String
  | ElementContent Element

data ElementSize = Pixel Int | Percent Number

data EmptyAttribute

newtype Attribute = Attribute
  { key          :: String
  , value        :: Maybe String
  }

newtype AttributeKey :: forall k. k -> Type
newtype AttributeKey a = AttributeKey String

element :: String -> Array Attribute -> Maybe (Array Content) -> Element
element name attribs content = Element
  { name:      name
  , attribs:   attribs
  , content:   content
  }

text :: String -> Content
text = TextContent

elem :: Element -> Content
elem = ElementContent

class IsValue a where
  toValue :: a -> String

instance stringIsValue :: IsValue String where
  toValue = identity

instance intIsValue :: IsValue Int where
  toValue = show

instance elementSizeIsValue :: IsValue ElementSize where
  toValue (Pixel n) = show n <> "px"
  toValue (Percent n) = show n <> "%"

instance emptyAttributeIsValue :: IsValue EmptyAttribute where
  toValue _ = ""

percent :: Number -> ElementSize
percent = Percent

pixel :: Int -> ElementSize
pixel = Pixel

attribute :: forall a. IsValue a => AttributeKey a -> a -> Attribute
attribute (AttributeKey key) value = Attribute
  { key: key
  , value: Just $ toValue value
  }

infix 4 attribute as :=


emptyAttribute :: AttributeKey EmptyAttribute -> Boolean -> Attribute
emptyAttribute (AttributeKey key) bool = Attribute
  { key: if bool then key else ""
  , value: Nothing
  }

infix 4 emptyAttribute as :~

a :: Array Attribute -> Array Content -> Element
a attribs content = element "a" attribs (Just content)

p :: Array Attribute -> Array Content -> Element
p attribs content = element "p" attribs (Just content)

img :: Array Attribute -> Element
img attribs = element "img" attribs Nothing

href :: AttributeKey String
href = AttributeKey "href"

_class :: AttributeKey String
_class = AttributeKey "class"

src :: AttributeKey String
src = AttributeKey "src"

width :: AttributeKey ElementSize
width = AttributeKey "width"

height :: AttributeKey ElementSize
height = AttributeKey "height"

disabled :: AttributeKey EmptyAttribute
disabled = AttributeKey "disabled"

checked :: AttributeKey EmptyAttribute
checked = AttributeKey "checked"

render :: Element -> String
render (Element e) =
    "<" <> e.name <>
    " " <> joinWith " " (map renderAttribute e.attribs) <>
    renderContent e.content
  where
    renderAttribute :: Attribute -> String
    renderAttribute (Attribute x)
      | isNothing x.value = x.key
      | otherwise = x.key <> "=\"" <> value <> "\"" where
        value = fromMaybe "" x.value

    renderContent :: Maybe (Array Content) -> String
    renderContent Nothing = " />"
    renderContent (Just content) =
        ">" <> joinWith "" (map renderContentItem content) <>
        "</" <> e.name <> ">"
      where
        renderContentItem :: Content -> String
        renderContentItem (TextContent s) = s
        renderContentItem (ElementContent e') = render e'

test = render $ img
    [ src    := "cat.jpg"
    , width  := pixel 100
    , height := percent 50.0
    , disabled :~ false
    ]
