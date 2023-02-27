module Blessed.UI.Base.Element.Property where

import Prelude

import Prim.Row as R
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)

import Blessed.Core.Offset (Offset)

import Blessed.Core.Style as Style
import Blessed.Core.Border as Border
-- import Blessed.Core.Position (Position)

import Data.Codec.Argonaut as CA
import Data.Argonaut.Decode (class DecodeJson)

import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.BlessedSubj (Subject, Element, class IsSubject, class Extends, element)
import Blessed.Internal.Core
    ( Getter
    , GetterFn, GetterFnC, GetterFn2, GetterFnC2
    , getter, getter2, getterC, getterC2
    , class Gets, class GetsC, class Gets2, class GetsC2
    ) as C



type PropertiesRow =
    ( name :: String
    , border :: Record Border.Evaluated
    , style :: Record Style.Evaluated
    -- , position :: TODO
    , content :: String
    , hidden :: Boolean
    , visible :: Boolean
    , detached :: Boolean
    , fg :: Int
    , bg :: Int
    , bold :: Boolean
    , underline :: Boolean
    , width :: Int
    , height :: Int
    , left :: Int
    , right :: Int
    , top :: Int
    , bottom :: Int
    , aleft :: Int
    , aright :: Int
    , atop :: Int
    , abottom :: Int
    , tags :: Boolean
    {- , box ::
        { left :: Int
        , top :: Int
        , bottom :: Int
        , right :: Int
        } -}

    , draggable :: Boolean
    , hover :: Record Style.Evaluated
    ) -- TODO + Node.Property


getter
    :: forall subj id prop r' state m a
     . C.Gets Element subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFn subj id prop state m a
getter =
    C.getter element


getterC
    :: forall subj id prop r' state m a
     . C.GetsC Element subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFnC subj id prop state m a
getterC =
    C.getterC element


getter2
    :: forall subj id propA propB ir' ir r' state m a
     . C.Gets2 Element subj id propA propB m a
    => R.Cons propA (Record ir) r' PropertiesRow
    => R.Cons propB a ir' ir
    => C.GetterFn2 subj id propA propB state m a
getter2 =
    C.getter2 element


getterC2
    :: forall subj id propA propB ir' ir r' state m a
     . C.GetsC2 Element subj id propA propB m a
    => R.Cons propA (Record ir) r' PropertiesRow
    => R.Cons propB a ir' ir
    => C.GetterFnC2 subj id propA propB state m a
getterC2 =
    C.getterC2 element


name
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "name" m String
    => NodeKey subj id -> C.Getter state m String
name = getterC (Proxy :: _ "name") CA.string


border
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.Gets Element subj id "border" m String
    => NodeKey subj id -> C.Getter state m (Record Border.Evaluated)
border = getter (Proxy :: _ "border")


style
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.Gets Element subj id "style" m (Record Style.Evaluated)
    => NodeKey subj id -> C.Getter state m (Record Style.Evaluated)
style = getter (Proxy :: _ "style")



content
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "content" m String
    => NodeKey subj id -> C.Getter state m String
content = getterC (Proxy :: _ "content") CA.string


hidden
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "hidden" m Boolean
    => NodeKey subj id -> C.Getter state m Boolean
hidden = getterC (Proxy :: _ "hidden") CA.boolean


visible
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "visible" m Boolean
    => NodeKey subj id -> C.Getter state m Boolean
visible = getterC (Proxy :: _ "visible") CA.boolean


detached
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "detached" m Boolean
    => NodeKey subj id -> C.Getter state m Boolean
detached = getterC (Proxy :: _ "detached") CA.boolean


fg
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "fg" m Int
    => NodeKey subj id -> C.Getter state m Int
fg = getterC (Proxy :: _ "fg") CA.int


bg
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "bg" m Int
    => NodeKey subj id -> C.Getter state m Int
bg = getterC (Proxy :: _ "bg") CA.int


bold
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "bold" m Boolean
    => NodeKey subj id -> C.Getter state m Boolean
bold = getterC (Proxy :: _ "bold") CA.boolean


underline
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "underline" m Boolean
    => NodeKey subj id -> C.Getter state m Boolean
underline = getterC (Proxy :: _ "underline") CA.boolean


width
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "width" m Int
    => NodeKey subj id -> C.Getter state m Int
width = getterC (Proxy :: _ "width") CA.int


height
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "height" m Int
    => NodeKey subj id -> C.Getter state m Int
height = getterC (Proxy :: _ "height") CA.int


left
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "left" m Int
    => NodeKey subj id -> C.Getter state m Int
left = getterC (Proxy :: _ "left") CA.int


right
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "right" m Int
    => NodeKey subj id -> C.Getter state m Int
right = getterC (Proxy :: _ "right") CA.int


top
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "top" m Int
    => NodeKey subj id -> C.Getter state m Int
top = getterC (Proxy :: _ "top") CA.int


bottom
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "bottom" m Int
    => NodeKey subj id -> C.Getter state m Int
bottom = getterC (Proxy :: _ "bottom") CA.int


aleft
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "aleft" m Int
    => NodeKey subj id -> C.Getter state m Int
aleft = getterC (Proxy :: _ "aleft") CA.int


aright
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "aright" m Int
    => NodeKey subj id -> C.Getter state m Int
aright = getterC (Proxy :: _ "aright") CA.int


atop
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "atop" m Int
    => NodeKey subj id -> C.Getter state m Int
atop = getterC (Proxy :: _ "atop") CA.int


abottom
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "abottom" m Int
    => NodeKey subj id -> C.Getter state m Int
abottom = getterC (Proxy :: _ "abottom") CA.int


tags
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "tags" m Boolean
    => NodeKey subj id -> C.Getter state m Boolean
tags = getterC (Proxy :: _ "tags") CA.boolean


draggable
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Element subj id "draggable" m Boolean
    => NodeKey subj id -> C.Getter state m Boolean
draggable = getterC (Proxy :: _ "draggable") CA.boolean


{- there's no such properties
boxLeft
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC2 Element subj id "box" "left" m Int
    => NodeKey subj id -> C.Getter state m Int
boxLeft = getterC2 (Proxy :: _ "box") (Proxy :: _ "left") CA.int


boxRight
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC2 Element subj id "box" "right" m Int
    => NodeKey subj id -> C.Getter state m Int
boxRight = getterC2 (Proxy :: _ "box") (Proxy :: _ "right") CA.int


boxTop
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC2 Element subj id "box" "top" m Int
    => NodeKey subj id -> C.Getter state m Int
boxTop = getterC2 (Proxy :: _ "box") (Proxy :: _ "top") CA.int


boxBottom
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC2 Element subj id "box" "bottom" m Int
    => NodeKey subj id -> C.Getter state m Int
boxBottom = getterC2 (Proxy :: _ "box") (Proxy :: _ "bottom") CA.int
-}