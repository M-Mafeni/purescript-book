module Data.DOM.Name
  ( Attribute
  , Name
  , Content
  , ContentF
  , AttributeKey
  , class IsValue
  , toValue
  , Href(..)

  , a
  , p
  , img

  , href
  , _class
  , src
  , width
  , height
  , name
  , isMobile

  , attribute, (:=)
  , text
  , newName

  , render
  , testVal
  , testVal1
  , testVal2
  ) where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (State, evalState)
import Control.Monad.State.Trans (put, get)
import Control.Monad.Writer.Trans (WriterT, execWriterT, tell)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))

newtype Element = Element
  { name         :: String
  , attribs      :: Array Attribute
  , content      :: Maybe (Content Unit)
  }

newtype Name = Name String

data ContentF a
  = TextContent String a
  | ElementContent Element a
  | NewName (Name -> a)
  | Mobile (Boolean -> a)

instance functorContentF :: Functor ContentF where
  map f (TextContent s x) = TextContent s (f x)
  map f (ElementContent e x) = ElementContent e (f x)
  map f (NewName k) = NewName (f <<< k)
  map f (Mobile k) = Mobile (f <<< k)

newtype Content a = Content (Free ContentF a)

fromContent :: forall a. Content a -> Free ContentF a
fromContent (Content c) = c

instance contentFunctor :: Functor Content where
  -- map :: (a -> b) -> Content a -> Content b
  map f (Content c) = Content (f <$> c)

instance contentApply :: Apply Content where
  -- apply :: Content (a -> b) -> Content a -> Content b
  apply (Content f) (Content x) = Content $ f <*> x


instance contentApplicative :: Applicative Content where
  -- pure :: a -> Content a
  pure x = Content $ pure x

instance contentBind :: Bind Content where
  -- bind :: Content a -> (a -> Content b) -> Content b
  -- (Free ContentF a) -> a
  bind (Content c) f = Content $ c >>= (fromContent <<< f)

newtype Attribute = Attribute
  { key          :: String
  , value        :: String
  }

newtype AttributeKey :: forall k. k -> Type
newtype AttributeKey a = AttributeKey String

element :: String -> Array Attribute -> Maybe (Content Unit) -> Element
element name_ attribs content = Element { name: name_, attribs, content }

text :: String -> Content Unit
text s = Content $ liftF $ TextContent s unit

elem :: Element -> Content Unit
elem e = Content $ liftF $ ElementContent e unit

newName :: Content Name
newName = Content $ liftF $ NewName identity

isMobile :: Content Boolean
isMobile = Content $ liftF $ Mobile identity

class IsValue a where
  toValue :: a -> String

instance stringIsValue :: IsValue String where
  toValue = identity

instance intIsValue :: IsValue Int where
  toValue = show

instance nameIsValue :: IsValue Name where
  toValue (Name n) = n

attribute :: forall a. IsValue a => AttributeKey a -> a -> Attribute
attribute (AttributeKey key) value = Attribute
  { key: key
  , value: toValue value
  }

infix 4 attribute as :=

a :: Array Attribute -> Content Unit -> Content Unit
a attribs content = elem $ element "a" attribs (Just content)

p :: Array Attribute -> Content Unit -> Content Unit
p attribs content = elem $ element "p" attribs (Just content)

img :: Array Attribute -> Content Unit
img attribs = elem $ element "img" attribs Nothing

data Href
  = URLHref String
  | AnchorHref Name

instance hrefIsValue :: IsValue Href where
  toValue (URLHref url) = url
  toValue (AnchorHref (Name nm)) = "#" <> nm

href :: AttributeKey Href
href = AttributeKey "href"

name :: AttributeKey Name
name = AttributeKey "name"

_class :: AttributeKey String
_class = AttributeKey "class"

src :: AttributeKey String
src = AttributeKey "src"

width :: AttributeKey Int
width = AttributeKey "width"

height :: AttributeKey Int
height = AttributeKey "height"

type Interp = ReaderT (Boolean) (WriterT String (State Int))
-- Content Unit = Content (Free ContentF Unit)
-- ContentF (Content a) -> Interp (Content a)
render :: Content Unit -> String
render (Content c) = evalState (execWriterT (runReaderT (runFreeM renderContentItem c) false)) 0 where
  renderContentItem :: forall a. ContentF (Free ContentF a) -> Interp (Free ContentF a)
  renderContentItem (TextContent s rest) = do
    tell s
    pure rest
  renderContentItem (ElementContent e' rest) = do
    renderElement e'
    pure rest
  renderContentItem (NewName k) = do
    n <- get
    let fresh = Name $ "name" <> show n
    put $ n + 1
    pure (k fresh)
  renderContentItem (Mobile f) = do
    onMobile <- ask
    pure (f onMobile)
  
  renderElement :: Element -> Interp Unit
  renderElement (Element e) = do
      tell "<"
      tell e.name
      for_ e.attribs $ \x -> do
        tell " "
        renderAttribute x
      renderContent e.content
    where
      renderAttribute :: Attribute -> Interp Unit
      renderAttribute (Attribute x) = do
        tell x.key
        tell "=\""
        tell x.value
        tell "\""

      renderContent :: Maybe (Content Unit) -> Interp Unit
      renderContent Nothing = tell " />"
      renderContent (Just (Content c1)) = do
        tell ">"
        runFreeM renderContentItem c1
        tell "</"
        tell  e.name
        tell ">"

testVal :: String
testVal = render $ p [] $ do
  img [ src := "cat.jpg" ]
  text "A cat"
  img [ src := "dog.jpg" ]

testVal1 :: String
testVal1 = render $ p [ ] $ do
   top <- newName
   a [ name := top ] $
     text "Top"
   a [ href := AnchorHref top ] $
     text "Back to top"

testVal2 :: String
testVal2 = render $ p [ ] $ do
  onMobile <- isMobile
  if onMobile
    then p [] $ text "I'm on a mobile device"
    else p [] $ text "I'm on a laptop"