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
import Blessed.Internal.Core (Getter, GetterFn, GetterFn', getter, getter') as C
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.BlessedSubj (Box, Element, Subject, class Extends)

import Blessed.UI.Base.Element.Property as E


type PropertiesRow = E.PropertiesRow


getter
    :: forall subj id sym r' m a
     . Respresents Box subj id
    => R.Cons sym a r' PropertiesRow
    => C.GetterFn subj id sym r' PropertiesRow m a
getter =
    C.getter


getter'
    :: forall subj id sym r' m a
     . DecodeJson a
    => Respresents Box subj id
    => R.Cons sym a r' PropertiesRow
    => C.GetterFn' subj id sym r' PropertiesRow m a
getter' =
    C.getter'


name
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m String
name = E.name


border
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m (Record Border.Evaluated)
border = E.border


style
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m (Record Style.Evaluated)
style = E.style


content
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m String
content = E.content


hidden
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m Boolean
hidden = E.hidden


visible
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m Boolean
visible = E.visible


detached
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m Boolean
detached = E.detached


fg
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m Int
fg = E.fg


bg
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m Int
bg = E.bg


bold
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m Boolean
bold = E.bold


underline
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m Boolean
underline = E.underline


width
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m Int
width = E.width


height
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m Int
height = E.height


left
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m Int
left = E.left


right
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m Int
right = E.right


top
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m Int
top = E.top


bottom
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m Int
bottom = E.bottom


aleft
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m Int
aleft = E.aleft


aright
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m Int
aright = E.aright


atop
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m Int
atop = E.atop


abottom
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m Int
abottom = E.abottom


tags
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m Boolean
tags = E.tags



draggable
    :: forall (subj :: Subject) (id :: Symbol) m
     . Extends Element subj => Respresents Box subj id
    => NodeKey subj id -> C.Getter m Boolean
draggable = E.draggable
