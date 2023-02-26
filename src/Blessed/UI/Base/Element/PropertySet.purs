module Blessed.UI.Base.Element.PropertySet where

import Prelude

import Prim.Row as R
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)

import Blessed.Core.Style as Style
import Blessed.Core.Border as Border
import Blessed.Core.Offset (Offset)
import Blessed.Core.Dimension (Dimension)
-- import Blessed.Core.Position (Position)

import Data.Codec.Argonaut as CA
import Data.Argonaut.Decode (class DecodeJson)

import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.BlessedSubj (Subject, Element, class IsSubject, class Extends, element)
import Blessed.Internal.Core
    ( Setter
    , SetterFn, SetterFnC, SetterFn2, SetterFnC2
    , setter, setter2, setterC, setterC2
    , class Sets, class SetsC, class Sets2, class SetsC2
    ) as C



type PropertiesRow =
    ( width :: Dimension
    , height :: Dimension
    , top :: Offset
    , left :: Offset
    ) -- TODO + Node.Property



setter
    :: forall subj id prop r' state m a
     . C.Sets Element subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.SetterFn subj id prop state m a
setter =
    C.setter element


setterC
    :: forall subj id prop r' state m a
     . C.SetsC Element subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.SetterFnC subj id prop state m a
setterC =
    C.setterC element


setter2
    :: forall subj id propA propB ir' ir r' state m a
     . C.Sets2 Element subj id propA propB m a
    => R.Cons propA (Record ir) r' PropertiesRow
    => R.Cons propB a ir' ir
    => C.SetterFn2 subj id propA propB state m a
setter2 =
    C.setter2 element


setterC2
    :: forall subj id propA propB ir' ir r' state m a
     . C.SetsC2 Element subj id propA propB m a
    => R.Cons propA (Record ir) r' PropertiesRow
    => R.Cons propB a ir' ir
    => C.SetterFnC2 subj id propA propB state m a
setterC2 =
    C.setterC2 element



setLeft
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.SetsC Element subj id "left" m Offset
    => Offset -> NodeKey subj id -> C.Setter state m Offset
setLeft = setter (Proxy :: _ "left")


setTop
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.SetsC Element subj id "top" m Offset
    => Offset -> NodeKey subj id -> C.Setter state m Offset
setTop = setter (Proxy :: _ "top")


setWidth
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.SetsC Element subj id "width" m Dimension
    => Dimension -> NodeKey subj id -> C.Setter state m Dimension
setWidth = setter (Proxy :: _ "width")


setHeight
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.SetsC Element subj id "height" m Dimension
    => Dimension -> NodeKey subj id -> C.Setter state m Dimension
setHeight = setter (Proxy :: _ "height")





-- TODO: other setters