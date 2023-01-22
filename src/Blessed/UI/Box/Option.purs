module Blessed.UI.Box.Option where

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

import Blessed.UI.Box.Event (Event)
import Blessed.UI.Element.Option as E


type OptionsRow r = E.OptionsRow r
type Options = Record (OptionsRow ())


type BoxAttribute r = C.Attribute (OptionsRow + r) Event


boxOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> BoxAttribute r
boxOption = C.option


fg :: forall r. Color -> BoxAttribute ( fg :: Color | r )
fg = E.fg


bg :: forall r. Color -> BoxAttribute ( bg :: Color | r )
bg = E.bg


bold :: forall r. Boolean -> BoxAttribute ( bold :: Boolean | r )
bold = E.bold


underline :: forall r. Boolean -> BoxAttribute ( underline :: Boolean | r )
underline = E.underline


style :: forall r. Array (StyleOption ()) -> BoxAttribute ( style :: Array (StyleOption ()) | r )
style = E.style


border :: forall r. Array (BorderOption ()) -> BoxAttribute ( border :: Array (BorderOption ()) | r )
border = E.border


content :: forall r. String -> BoxAttribute ( content :: String | r )
content = E.content


clickable :: forall r. Boolean -> BoxAttribute ( clickable :: Boolean | r )
clickable = E.clickable


input :: forall r. Boolean -> BoxAttribute ( input :: Boolean | r )
input = E.input


keyable :: forall r. Boolean -> BoxAttribute ( keyable :: Boolean | r )
keyable = E.keyable


focused :: forall r. Boolean -> BoxAttribute ( focused :: Boolean | r )
focused = E.focused


hidden :: forall r. Boolean -> BoxAttribute ( hidden :: Boolean | r )
hidden = E.hidden


label :: forall r. String -> BoxAttribute ( label :: String | r )
label = E.label


hoverText :: forall r. String -> BoxAttribute ( hoverText :: String | r )
hoverText = E.hoverText


align :: forall r. HAlign -> BoxAttribute ( align :: HAlign | r )
align = E.align


valign :: forall r. VAlign -> BoxAttribute ( valign :: VAlign | r )
valign = E.valign


shrink :: forall r. Flex -> BoxAttribute ( shrink :: Flex | r )
shrink = E.shrink


padding :: forall r. Padding -> BoxAttribute ( padding :: Padding | r )
padding = E.padding


width :: forall r. Dimension -> BoxAttribute ( width :: Dimension | r )
width = E.width


height :: forall r. Dimension -> BoxAttribute ( height :: Dimension | r )
height = E.height


left :: forall r. Offset -> BoxAttribute ( left :: Offset | r )
left = E.left


right :: forall r. Offset -> BoxAttribute ( right :: Offset | r )
right = E.right


top :: forall r. Offset -> BoxAttribute ( top :: Offset | r )
top = E.top


bottom :: forall r. Offset -> BoxAttribute ( bottom :: Offset | r )
bottom = E.bottom


ch :: forall r. Char -> BoxAttribute ( ch :: Char | r )
ch = E.ch


draggable :: forall r. Boolean -> BoxAttribute ( draggable :: Boolean | r )
draggable = E.draggable


shadow :: forall r. Boolean -> BoxAttribute ( shadow :: Boolean | r )
shadow = E.shadow


tags :: forall r. Boolean -> BoxAttribute ( tags :: Boolean | r )
tags = E.tags
