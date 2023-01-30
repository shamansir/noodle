module Blessed.UI.Base.Element.Property where

import Prelude

import Prim.Row as R
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)

import Blessed.Core.Style as Style
import Blessed.Core.Border as Border
-- import Blessed.Core.Position (Position)

import Data.Codec.Argonaut as CA
import Data.Argonaut.Decode (class DecodeJson)

import Blessed.Internal.NodeKey (NodeKey, class Represents)
import Blessed.Internal.BlessedSubj (Element, class IsSubject)
import Blessed.Internal.Core (GetterFn, GetterFn', Getter, getter, getter') as C



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

    , draggable :: Boolean
    , hover :: Record Style.Evaluated
    ) -- TODO + Node.Property


getter :: forall subj id sym r' m a. Represents Element subj id => R.Cons sym a r' PropertiesRow => C.GetterFn subj id sym r' PropertiesRow m a
getter =
    C.getter


getter' :: forall subj id sym r' m a. Represents Element subj id => IsSubject subj => IsSymbol id => DecodeJson a => R.Cons sym a r' PropertiesRow => C.GetterFn' subj id sym r' PropertiesRow m a
getter' =
    C.getter'


name
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m String
name = getter (Proxy :: _ "name") CA.string


border
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m (Record Border.Evaluated)
border = getter' (Proxy :: _ "border")


style
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m (Record Style.Evaluated)
style = getter' (Proxy :: _ "style")


content
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m String
content = getter (Proxy :: _ "content") CA.string


hidden
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m Boolean
hidden = getter (Proxy :: _ "hidden") CA.boolean


visible
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m Boolean
visible = getter (Proxy :: _ "visible") CA.boolean


detached
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m Boolean
detached = getter (Proxy :: _ "detached") CA.boolean


fg
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m Int
fg = getter (Proxy :: _ "fg") CA.int


bg
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m Int
bg = getter (Proxy :: _ "bg") CA.int


bold
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m Boolean
bold = getter (Proxy :: _ "bold") CA.boolean


underline
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m Boolean
underline = getter (Proxy :: _ "underline") CA.boolean


width
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m Int
width = getter (Proxy :: _ "width") CA.int


height
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m Int
height = getter (Proxy :: _ "height") CA.int


left
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m Int
left = getter (Proxy :: _ "left") CA.int


right
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m Int
right = getter (Proxy :: _ "right") CA.int


top
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m Int
top = getter (Proxy :: _ "top") CA.int


bottom
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m Int
bottom = getter (Proxy :: _ "bottom") CA.int


aleft
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m Int
aleft = getter (Proxy :: _ "aleft") CA.int


aright
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m Int
aright = getter (Proxy :: _ "aright") CA.int


atop
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m Int
atop = getter (Proxy :: _ "atop") CA.int


abottom
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m Int
abottom = getter (Proxy :: _ "abottom") CA.int


tags
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m Boolean
tags = getter (Proxy :: _ "tags") CA.boolean



draggable
    :: forall subj id m
     . Represents Element subj id
    => NodeKey subj id -> C.Getter m Boolean
draggable = getter (Proxy :: _ "draggable") CA.boolean
