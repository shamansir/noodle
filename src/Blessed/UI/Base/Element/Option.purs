module Blessed.UI.Base.Element.Option where

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

import Blessed.UI.Boxes.Box.Event (Event)


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


type ElementAttribute r e = C.Attribute (OptionsRow + r) e


elmOption :: forall a r r' sym e. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> ElementAttribute r e
elmOption = C.option


fg :: forall r e. Color -> ElementAttribute ( fg :: Color | r ) e
fg = elmOption (Proxy :: _ "fg")


bg :: forall r e. Color -> ElementAttribute ( bg :: Color | r ) e
bg = elmOption (Proxy :: _ "bg")


bold :: forall r e. Boolean -> ElementAttribute ( bold :: Boolean | r ) e
bold = elmOption (Proxy :: _ "bold")


underline :: forall r e. Boolean -> ElementAttribute ( underline :: Boolean | r ) e
underline = elmOption (Proxy :: _ "underline")


style :: forall r e. Array (StyleOption ()) -> ElementAttribute ( style :: Array (StyleOption ()) | r ) e
style = elmOption (Proxy :: _ "style")


border :: forall r e. Array (BorderOption ()) -> ElementAttribute ( border :: Array (BorderOption ()) | r ) e
border = elmOption (Proxy :: _ "border")


content :: forall r e. String -> ElementAttribute ( content :: String | r ) e
content = elmOption (Proxy :: _ "content")


clickable :: forall r e. Boolean -> ElementAttribute ( clickable :: Boolean | r ) e
clickable = elmOption (Proxy :: _ "clickable")


input :: forall r e. Boolean -> ElementAttribute ( input :: Boolean | r ) e
input = elmOption (Proxy :: _ "input")


keyable :: forall r e. Boolean -> ElementAttribute ( keyable :: Boolean | r ) e
keyable = elmOption (Proxy :: _ "keyable")


focused :: forall r e. Boolean -> ElementAttribute ( focused :: Boolean | r ) e
focused = elmOption (Proxy :: _ "focused")


hidden :: forall r e. Boolean -> ElementAttribute ( hidden :: Boolean | r ) e
hidden = elmOption (Proxy :: _ "hidden")


label :: forall r e. String -> ElementAttribute ( label :: String | r ) e
label = elmOption (Proxy :: _ "label")


hoverText :: forall r e. String -> ElementAttribute ( hoverText :: String | r ) e
hoverText = elmOption (Proxy :: _ "hoverText")


align :: forall r e. HAlign -> ElementAttribute ( align :: HAlign | r ) e
align = elmOption (Proxy :: _ "align")


valign :: forall r e. VAlign -> ElementAttribute ( valign :: VAlign | r ) e
valign = elmOption (Proxy :: _ "valign")


shrink :: forall r e. Flex -> ElementAttribute ( shrink :: Flex | r ) e
shrink = elmOption (Proxy :: _ "shrink")


padding :: forall r e. Padding -> ElementAttribute ( padding :: Padding | r ) e
padding = elmOption (Proxy :: _ "padding")


width :: forall r e. Dimension -> ElementAttribute ( width :: Dimension | r ) e
width = elmOption (Proxy :: _ "width")


height :: forall r e. Dimension -> ElementAttribute ( height :: Dimension | r ) e
height = elmOption (Proxy :: _ "height")


left :: forall r e. Offset -> ElementAttribute ( left :: Offset | r ) e
left = elmOption (Proxy :: _ "left")


right :: forall r e. Offset -> ElementAttribute ( right :: Offset | r ) e
right = elmOption (Proxy :: _ "right")


top :: forall r e. Offset -> ElementAttribute ( top :: Offset | r ) e
top = elmOption (Proxy :: _ "top")


bottom :: forall r e. Offset -> ElementAttribute ( bottom :: Offset | r ) e
bottom = elmOption (Proxy :: _ "bottom")


ch :: forall r e. Char -> ElementAttribute ( ch :: Char | r ) e
ch = elmOption (Proxy :: _ "ch")


draggable :: forall r e. Boolean -> ElementAttribute ( draggable :: Boolean | r ) e
draggable = elmOption (Proxy :: _ "draggable")


shadow :: forall r e. Boolean -> ElementAttribute ( shadow :: Boolean | r ) e
shadow = elmOption (Proxy :: _ "shadow")


tags :: forall r e. Boolean -> ElementAttribute ( tags :: Boolean | r ) e
tags = elmOption (Proxy :: _ "tags")
