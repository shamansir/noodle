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

import Blessed.Internal.Core (NodeId, GetterFn, GetterFn', Getter, getter, getter') as C
import Blessed.Internal.BlessedOp as Op
import Blessed.Internal.Command (get) as C

import Blessed.UI.Base.Element.Property as E


type PropertiesRow = E.PropertiesRow


getter :: forall sym r' m a. R.Cons sym a r' PropertiesRow => C.GetterFn sym r' PropertiesRow m a
getter =
    C.getter


getter' :: forall sym r' m a. DecodeJson a => R.Cons sym a r' PropertiesRow => C.GetterFn' sym r' PropertiesRow m a
getter' =
    C.getter'


name :: forall m. C.NodeId -> C.Getter m String
name = E.name


border :: forall m. C.NodeId -> C.Getter m (Record Border.Evaluated)
border = E.border


style :: forall m. C.NodeId -> C.Getter m (Record Style.Evaluated)
style = E.style


content :: forall m. C.NodeId -> C.Getter m String
content = E.content


hidden :: forall m. C.NodeId -> C.Getter m Boolean
hidden = E.hidden


visible :: forall m. C.NodeId -> C.Getter m Boolean
visible = E.visible


detached :: forall m. C.NodeId -> C.Getter m Boolean
detached = E.detached


fg :: forall m. C.NodeId -> C.Getter m Int
fg = E.fg


bg :: forall m. C.NodeId -> C.Getter m Int
bg = E.bg


bold :: forall m. C.NodeId -> C.Getter m Boolean
bold = E.bold


underline :: forall m. C.NodeId -> C.Getter m Boolean
underline = E.underline


width :: forall m. C.NodeId -> C.Getter m Int
width = E.width


height :: forall m. C.NodeId -> C.Getter m Int
height = E.height


left :: forall m. C.NodeId -> C.Getter m Int
left = E.left


right :: forall m. C.NodeId -> C.Getter m Int
right = E.right


top :: forall m. C.NodeId -> C.Getter m Int
top = E.top


bottom :: forall m. C.NodeId -> C.Getter m Int
bottom = E.bottom


aleft :: forall m. C.NodeId -> C.Getter m Int
aleft = E.aleft


aright :: forall m. C.NodeId -> C.Getter m Int
aright = E.aright


atop :: forall m. C.NodeId -> C.Getter m Int
atop = E.atop


abottom :: forall m. C.NodeId -> C.Getter m Int
abottom = E.abottom


tags :: forall m. C.NodeId -> C.Getter m Boolean
tags = E.tags



draggable :: forall m. C.NodeId -> C.Getter m Boolean
draggable = E.draggable
