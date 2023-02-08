module Blessed.UI.Boxes.Box.Property where

import Prelude

import Prim.Row as R
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)

import Blessed.Core.Style (Style)
import Blessed.Core.Border (Border)
import Blessed.Core.Style as Style
import Blessed.Core.Border as Border


import Data.Codec.Argonaut as CA
import Data.Argonaut.Decode (class DecodeJson)

import Blessed.Internal.BlessedOp as Op
import Blessed.Internal.Command (get) as C
import Blessed.Internal.Core as C
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.BlessedSubj (box, Box, Element, Subject, class Extends)

import Blessed.UI.Base.Element.Property as E


type PropertiesRow = E.PropertiesRow


getter
    :: forall subj id prop r' state m a
     . Extends Element subj
    => C.Gets Box subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFn subj id prop state m a
getter =
    C.getter box


getterC
    :: forall subj id prop r' state m a
     . Extends Element subj
    => C.GetsC Box subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFnC subj id prop state m a
getterC =
    C.getterC box


name
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "name" m String
    => C.GetsC Box subj id "name" m String
    => NodeKey subj id -> C.Getter state m String
name = E.name


border
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "border" m (Record Border.Evaluated)
    => C.GetsC Box subj id "border" m (Record Border.Evaluated)
    => NodeKey subj id -> C.Getter state m (Record Border.Evaluated)
border = E.border


style
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "style" m (Record Style.Evaluated)
    => C.GetsC Box subj id "style" m (Record Style.Evaluated)
    => NodeKey subj id -> C.Getter state m (Record Style.Evaluated)
style = E.style


content
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "content" m String
    => C.GetsC Box subj id "content" m String
    => NodeKey subj id -> C.Getter state m String
content = E.content


hidden
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "hidden" m Boolean
    => C.GetsC Box subj id "hidden" m Boolean
    => NodeKey subj id -> C.Getter state m Boolean
hidden = E.hidden


visible
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "visible" m Boolean
    => C.GetsC Box subj id "visible" m Boolean
    => NodeKey subj id -> C.Getter state m Boolean
visible = E.visible


detached
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "detached" m Boolean
    => C.GetsC Box subj id "detached" m Boolean
    => NodeKey subj id -> C.Getter state m Boolean
detached = E.detached


fg
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "fg" m Int
    => C.GetsC Box subj id "fg" m Int
    => NodeKey subj id -> C.Getter state m Int
fg = E.fg


bg
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "bg" m Int
    => C.GetsC Box subj id "bg" m Int
    => NodeKey subj id -> C.Getter state m Int
bg = E.bg


bold
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "bold" m Boolean
    => C.GetsC Box subj id "bold" m Boolean
    => NodeKey subj id -> C.Getter state m Boolean
bold = E.bold


underline
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "underline" m Boolean
    => C.GetsC Box subj id "underline" m Boolean
    => NodeKey subj id -> C.Getter state m Boolean
underline = E.underline


width
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "width" m Int
    => C.GetsC Box subj id "width" m Int
    => NodeKey subj id -> C.Getter state m Int
width = E.width


height
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "height" m Int
    => C.GetsC Box subj id "height" m Int
    => NodeKey subj id -> C.Getter state m Int
height = E.height


left
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "left" m Int
    => C.GetsC Box subj id "left" m Int
    => NodeKey subj id -> C.Getter state m Int
left = E.left


right
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "right" m Int
    => C.GetsC Box subj id "right" m Int
    => NodeKey subj id -> C.Getter state m Int
right = E.right


top
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "top" m Int
    => C.GetsC Box subj id "top" m Int
    => NodeKey subj id -> C.Getter state m Int
top = E.top


bottom
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "bottom" m Int
    => C.GetsC Box subj id "bottom" m Int
    => NodeKey subj id -> C.Getter state m Int
bottom = E.bottom


aleft
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "aleft" m Int
    => C.GetsC Box subj id "aleft" m Int
    => NodeKey subj id -> C.Getter state m Int
aleft = E.aleft


aright
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "aright" m Int
    => C.GetsC Box subj id "aright" m Int
    => NodeKey subj id -> C.Getter state m Int
aright = E.aright


atop
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "atop" m Int
    => C.GetsC Box subj id "atop" m Int
    => NodeKey subj id -> C.Getter state m Int
atop = E.atop


abottom
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "abottom" m Int
    => C.GetsC Box subj id "abottom" m Int
    => NodeKey subj id -> C.Getter state m Int
abottom = E.abottom


tags
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "tags" m Boolean
    => C.GetsC Box subj id "tags" m Boolean
    => NodeKey subj id -> C.Getter state m Boolean
tags = E.tags



draggable
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "draggable" m Boolean
    => C.GetsC Box subj id "draggable" m Boolean
    => NodeKey subj id -> C.Getter state m Boolean
draggable = E.draggable
