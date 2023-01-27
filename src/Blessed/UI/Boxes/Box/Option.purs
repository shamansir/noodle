module Blessed.UI.Boxes.Box.Option where

import Prelude ((<<<))

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

import Blessed.UI.Base.Element.Option as E


type OptionsRow r = E.OptionsRow r
type Options = Record (OptionsRow ())


type BoxAttribute r e = C.Attribute (OptionsRow + r) e
-- don't lock to the Box events here (and to their own events everywhere), locking is only needed for event handlers?


boxOption :: forall a r r' sym e. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> BoxAttribute r e
boxOption = C.option


fg :: forall r e. Color -> BoxAttribute ( fg :: Color | r ) e
fg = E.fg


bg :: forall r e. Color -> BoxAttribute ( bg :: Color | r ) e
bg = E.bg


bold :: forall r e. Boolean -> BoxAttribute ( bold :: Boolean | r ) e
bold = E.bold


underline :: forall r e. Boolean -> BoxAttribute ( underline :: Boolean | r ) e
underline = E.underline


style :: forall r e. Array (StyleOption ()) -> BoxAttribute ( style :: Array (StyleOption ()) | r ) e
style = E.style


border :: forall r e. Array (BorderOption ()) -> BoxAttribute ( border :: Array (BorderOption ()) | r ) e
border = E.border


content :: forall r e. String -> BoxAttribute ( content :: String | r ) e
content = E.content


clickable :: forall r e. Boolean -> BoxAttribute ( clickable :: Boolean | r ) e
clickable = E.clickable


input :: forall r e. Boolean -> BoxAttribute ( input :: Boolean | r ) e
input = E.input


keyable :: forall r e. Boolean -> BoxAttribute ( keyable :: Boolean | r ) e
keyable = E.keyable


focused :: forall r e. Boolean -> BoxAttribute ( focused :: Boolean | r ) e
focused = E.focused


hidden :: forall r e. Boolean -> BoxAttribute ( hidden :: Boolean | r ) e
hidden = E.hidden


label :: forall r e. String -> BoxAttribute ( label :: String | r ) e
label = E.label


hoverText :: forall r e. String -> BoxAttribute ( hoverText :: String | r ) e
hoverText = E.hoverText


align :: forall r e. HAlign -> BoxAttribute ( align :: HAlign | r ) e
align = E.align


valign :: forall r e. VAlign -> BoxAttribute ( valign :: VAlign | r ) e
valign = E.valign


shrink :: forall r e. Flex -> BoxAttribute ( shrink :: Flex | r ) e
shrink = E.shrink


padding :: forall r e. Padding -> BoxAttribute ( padding :: Padding | r ) e
padding = E.padding


width :: forall r e. Dimension -> BoxAttribute ( width :: Dimension | r ) e
width = E.width


height :: forall r e. Dimension -> BoxAttribute ( height :: Dimension | r ) e
height = E.height


left :: forall r e. Offset -> BoxAttribute ( left :: Offset | r ) e
left = E.left


right :: forall r e. Offset -> BoxAttribute ( right :: Offset | r ) e
right = E.right


top :: forall r e. Offset -> BoxAttribute ( top :: Offset | r ) e
top = E.top


bottom :: forall r e. Offset -> BoxAttribute ( bottom :: Offset | r ) e
bottom = E.bottom


ch :: forall r e. Char -> BoxAttribute ( ch :: Char | r ) e
ch = E.ch


draggable :: forall r e. Boolean -> BoxAttribute ( draggable :: Boolean | r ) e
draggable = E.draggable


shadow :: forall r e. Boolean -> BoxAttribute ( shadow :: Boolean | r ) e
shadow = E.shadow


tags :: forall r e. Boolean -> BoxAttribute ( tags :: Boolean | r ) e
tags = E.tags
