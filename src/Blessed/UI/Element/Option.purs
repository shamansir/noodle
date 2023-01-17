module Blessed.UI.Element.Option where

import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Offset (Offset)
import Blessed.Core.Color (Color)
import Blessed.Core.Padding (Padding)
import Blessed.Core.Flex (Flex)
import Blessed.Core.Align (HAlign, VAlign)
import Blessed.Core.Style (StyleOption)
import Blessed.Core.Border (BorderOption)

import Blessed.Internal.Core (Attribute, option) as C

import Blessed.UI.Box.Event (Event)


type OptionsRow r =
    ( fg :: Color
    , bg :: Color
    , bold :: Boolean
    , underline :: Boolean
    , style :: Array (StyleOption ())
    , border :: Array (BorderOption ())
    , content :: String -- a ?
    , clickable :: Boolean
    , input :: Boolean
    , keyable :: Boolean
    , focused :: Boolean
    , hidden :: Boolean
    , label :: String
    , hoverText :: String
    , align :: HAlign
    , valign :: VAlign
    , shrink :: Flex
    , padding :: Padding
    , width :: Dimension
    , height :: Dimension
    , left :: Offset
    , right :: Offset
    , top :: Offset
    , bottom :: Offset
    -- , position ::
    , scrollable :: Boolean
    , ch :: Char
    , draggable :: Boolean
    , shadow :: Boolean


    , tags :: Boolean
    , hover :: Array (StyleOption ())
    | r
    )
type Options = Record (OptionsRow ())


type ElementAttribute r = C.Attribute (OptionsRow + r) Event


elmOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> ElementAttribute r
elmOption = C.option


fg :: forall r. Color -> ElementAttribute ( fg :: Color | r)
fg = elmOption (Proxy :: _ "fg")


bg :: forall r. Color -> ElementAttribute ( bg :: Color | r)
bg = elmOption (Proxy :: _ "bg")


bold :: forall r. Boolean -> ElementAttribute ( bold :: Boolean | r)
bold = elmOption (Proxy :: _ "bold")


underline :: forall r. Boolean -> ElementAttribute ( underline :: Boolean | r)
underline = elmOption (Proxy :: _ "underline")


style :: forall r. Array (StyleOption ()) -> ElementAttribute ( style :: Array (StyleOption ()) | r)
style = elmOption (Proxy :: _ "style")


border :: forall r. Array (BorderOption ()) -> ElementAttribute ( border :: Array (BorderOption ()) | r)
border = elmOption (Proxy :: _ "border")


content :: forall r. String -> ElementAttribute ( content :: String | r)
content = elmOption (Proxy :: _ "content")


clickable :: forall r. Boolean -> ElementAttribute ( clickable :: Boolean | r)
clickable = elmOption (Proxy :: _ "clickable")


input :: forall r. Boolean -> ElementAttribute ( input :: Boolean | r)
input = elmOption (Proxy :: _ "input")


keyable :: forall r. Boolean -> ElementAttribute ( keyable :: Boolean | r)
keyable = elmOption (Proxy :: _ "keyable")


focused :: forall r. Boolean -> ElementAttribute ( focused :: Boolean | r)
focused = elmOption (Proxy :: _ "focused")


hidden :: forall r. Boolean -> ElementAttribute ( hidden :: Boolean | r)
hidden = elmOption (Proxy :: _ "hidden")


label :: forall r. String -> ElementAttribute ( label :: String | r)
label = elmOption (Proxy :: _ "label")


hoverText :: forall r. String -> ElementAttribute ( hoverText :: String | r)
hoverText = elmOption (Proxy :: _ "hoverText")


align :: forall r. HAlign -> ElementAttribute ( align :: HAlign | r)
align = elmOption (Proxy :: _ "align")


valign :: forall r. VAlign -> ElementAttribute ( valign :: VAlign | r)
valign = elmOption (Proxy :: _ "valign")


shrink :: forall r. Flex -> ElementAttribute ( shrink :: Flex | r)
shrink = elmOption (Proxy :: _ "shrink")


padding :: forall r. Padding -> ElementAttribute ( padding :: Padding | r)
padding = elmOption (Proxy :: _ "padding")


width :: forall r. Dimension -> ElementAttribute ( width :: Dimension | r)
width = elmOption (Proxy :: _ "width")


height :: forall r. Dimension -> ElementAttribute ( height :: Dimension | r)
height = elmOption (Proxy :: _ "height")


left :: forall r. Offset -> ElementAttribute ( left :: Offset | r)
left = elmOption (Proxy :: _ "left")


right :: forall r. Offset -> ElementAttribute ( right :: Offset | r)
right = elmOption (Proxy :: _ "right")


top :: forall r. Offset -> ElementAttribute ( top :: Offset | r)
top = elmOption (Proxy :: _ "top")


bottom :: forall r. Offset -> ElementAttribute ( bottom :: Offset | r)
bottom = elmOption (Proxy :: _ "bottom")


ch :: forall r. Char -> ElementAttribute ( ch :: Char | r)
ch = elmOption (Proxy :: _ "ch")


draggable :: forall r. Boolean -> ElementAttribute ( draggable :: Boolean | r)
draggable = elmOption (Proxy :: _ "draggable")


shadow :: forall r. Boolean -> ElementAttribute ( shadow :: Boolean | r)
shadow = elmOption (Proxy :: _ "shadow")


tags :: forall r. Boolean -> ElementAttribute ( tags :: Boolean | r)
tags = elmOption (Proxy :: _ "tags")
