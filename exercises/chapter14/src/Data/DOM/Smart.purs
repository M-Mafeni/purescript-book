module Data.DOM.Smart
  ( Element
  , Attribute
  , Content
  , AttributeKey
  , EmptyAttributeKey

  , a
  , p
  , img
  , input
  , form

  , href
  , _class
  , src
  , width
  , height

  , disabled

  , attribute, (:=)
  , text
  , elem
  , emptyAttribute

  , render
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith, null)

newtype Element = Element
  { name         :: String
  , attribs      :: Array Attribute
  , content      :: Maybe (Array Content)
  }

data Content
  = TextContent String
  | ElementContent Element

newtype Attribute = Attribute
  { key          :: String
  , value        :: Maybe String
  }

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

newtype AttributeKey = AttributeKey String

newtype EmptyAttributeKey = EmptyAttributeKey String

attribute :: AttributeKey -> String -> Attribute
attribute (AttributeKey key) value = Attribute
  { key: key
  , value: Just value
  }

emptyAttribute :: EmptyAttributeKey -> Attribute
emptyAttribute (EmptyAttributeKey key) = Attribute
  { key: key
  , value: Nothing
  }

infix 4 attribute as :=

a :: Array Attribute -> Array Content -> Element
a attribs content = element "a" attribs (Just content)

p :: Array Attribute -> Array Content -> Element
p attribs content = element "p" attribs (Just content)

img :: Array Attribute -> Element
img attribs = element "img" attribs Nothing

input :: Array Attribute -> Element
input attribs = element "input" attribs Nothing

form :: Array Attribute -> Array Content -> Element
form attribs content = element "form" attribs (Just content)

href :: AttributeKey
href = AttributeKey "href"

_class :: AttributeKey
_class = AttributeKey "class"

src :: AttributeKey
src = AttributeKey "src"

width :: AttributeKey
width = AttributeKey "width"

height :: AttributeKey
height = AttributeKey "height"

disabled :: EmptyAttributeKey
disabled = EmptyAttributeKey "disabled"

render :: Element -> String
render (Element e) =
    "<" <> e.name <>
    " " <> joinWith " " (map renderAttribute e.attribs) <>
    renderContent e.content
  where
    renderAttribute :: Attribute -> String
    renderAttribute (Attribute x) = 
      let
        value = fromMaybe "" x.value
      in
        if (null value) then x.key else x.key <> "=\"" <> value <> "\""

    renderContent :: Maybe (Array Content) -> String
    renderContent Nothing = " />"
    renderContent (Just content) =
        ">" <> joinWith "" (map renderContentItem content) <>
        "</" <> e.name <> ">"
      where
        renderContentItem :: Content -> String
        renderContentItem (TextContent s) = s
        renderContentItem (ElementContent e') = render e'
