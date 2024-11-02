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
import Blessed.Internal.BlessedSubj (Subject, Element, Box, class Extends)
import Blessed.Internal.NodeKey (class Respresents)

import Blessed.UI.Base.Element.Option as E


type OptionsRow r = E.OptionsRow r
type Options = Record (OptionsRow ())


type BoxAttribute subj id r state e = C.Attribute subj id (OptionsRow + r) state e
-- don't lock to the Box events here (and to their own events everywhere), locking is only needed for event handlers?


boxOption
    :: forall subj id a r r' sym state e
     . Extends Element subj => Respresents Box subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> BoxAttribute subj id r state e
boxOption = C.option


fg
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Color -> BoxAttribute subj id ( fg :: Color | r ) state e
fg = E.fg


bg
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Color -> BoxAttribute subj id ( bg :: Color | r ) state e
bg = E.bg


bold
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Boolean -> BoxAttribute subj id ( bold :: Boolean | r ) state e
bold = E.bold


underline
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Boolean -> BoxAttribute subj id ( underline :: Boolean | r ) state e
underline = E.underline


type StyleAttrubute subj id state e r = BoxAttribute subj id ( style :: Array (StyleOption ()) | r ) state e


style
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Array (StyleOption ()) -> StyleAttrubute subj id state e r
style = E.style


type BorderAttrubute subj id state e r = BoxAttribute subj id ( border :: Array (BorderOption ()) | r ) state e


border
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Array (BorderOption ()) -> BorderAttrubute subj id state e r
border = E.border


content
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => String -> BoxAttribute subj id ( content :: String | r ) state e
content = E.content


clickable
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Boolean -> BoxAttribute subj id ( clickable :: Boolean | r ) state e
clickable = E.clickable


input
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Boolean -> BoxAttribute subj id ( input :: Boolean | r ) state e
input = E.input


keyable
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Boolean -> BoxAttribute subj id ( keyable :: Boolean | r ) state e
keyable = E.keyable


focused
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Boolean -> BoxAttribute subj id ( focused :: Boolean | r ) state e
focused = E.focused


hidden
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Boolean -> BoxAttribute subj id ( hidden :: Boolean | r ) state e
hidden = E.hidden


label
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => String -> BoxAttribute subj id ( label :: String | r ) state e
label = E.label


hoverText
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => String -> BoxAttribute subj id ( hoverText :: String | r ) state e
hoverText = E.hoverText


align
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => HAlign -> BoxAttribute subj id ( align :: HAlign | r ) state e
align = E.align


valign
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => VAlign -> BoxAttribute subj id ( valign :: VAlign | r ) state e
valign = E.valign


shrink
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Flex -> BoxAttribute subj id ( shrink :: Flex | r ) state e
shrink = E.shrink


padding
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Padding -> BoxAttribute subj id ( padding :: Padding | r ) state e
padding = E.padding


width
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Dimension -> BoxAttribute subj id ( width :: Dimension | r ) state e
width = E.width


height
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Dimension -> BoxAttribute subj id ( height :: Dimension | r ) state e
height = E.height


left
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Offset -> BoxAttribute subj id ( left :: Offset | r ) state e
left = E.left


right
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Offset -> BoxAttribute subj id ( right :: Offset | r ) state e
right = E.right


top
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Offset -> BoxAttribute subj id ( top :: Offset | r ) state e
top = E.top


bottom
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Offset -> BoxAttribute subj id ( bottom :: Offset | r ) state e
bottom = E.bottom


ch
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Char -> BoxAttribute subj id ( ch :: Char | r ) state e
ch = E.ch


draggable
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Boolean -> BoxAttribute subj id ( draggable :: Boolean | r ) state e
draggable = E.draggable


scrollable
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Boolean -> BoxAttribute subj id ( scrollable :: Boolean | r ) state e
scrollable = E.scrollable


shadow
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Boolean -> BoxAttribute subj id ( shadow :: Boolean | r ) state e
shadow = E.shadow


tags
    :: forall (subj :: Subject) (id :: Symbol) r state e
     . Extends Element subj => Respresents Box subj id
    => Boolean -> BoxAttribute subj id ( tags :: Boolean | r ) state e
tags = E.tags
