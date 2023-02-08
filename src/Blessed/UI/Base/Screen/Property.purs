module Blessed.UI.Base.Screen.Property where

import Prelude

import Prim.Row as R
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Maybe (Maybe(..))

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.Core as C
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.BlessedSubj (screen, Screen, Subject)


-- newtype Focused = Focused String


type PropertiesRow =
    -- program ::
    -- tput ::
    ( focused :: Maybe String -- ?
    , width :: Int
    , height :: Int
    , cols :: Int
    , rows :: Int
    , left :: Int
    , right :: Int
    , top :: Int
    , bottom :: Int
    , aleft :: Int
    , aright :: Int
    , atop :: Int
    , abottom :: Int
    , grabKeys :: Boolean
    , lockKeys :: Boolean
    , hover :: Maybe String -- ?
    , terminal :: String
    , title :: String
    )


getter
    :: forall subj id prop r' state m a
     . C.Gets Screen subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFn subj id prop state m a
getter =
    C.getter screen


getterC
    :: forall subj id prop r' state m a
     . C.GetsC Screen subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFnC subj id prop state m a
getterC =
    C.getterC screen


focused
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Screen subj id "focused" m (Maybe String)
    => NodeKey subj id -> C.Getter state m (Maybe String)
focused = getterC (Proxy :: _ "focused") (CAC.maybe CA.string)


width
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Screen subj id "width" m Int
    => NodeKey subj id -> C.Getter state m Int
width = getterC (Proxy :: _ "width") CA.int


height
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Screen subj id "height" m Int
    => NodeKey subj id -> C.Getter state m Int
height = getterC (Proxy :: _ "height") CA.int


cols
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Screen subj id "cols" m Int
    => NodeKey subj id -> C.Getter state m Int
cols = getterC (Proxy :: _ "cols") CA.int


rows
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Screen subj id "rows" m Int
    => NodeKey subj id -> C.Getter state m Int
rows = getterC (Proxy :: _ "rows") CA.int


left
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Screen subj id "left" m Int
    => NodeKey subj id -> C.Getter state m Int
left = getterC (Proxy :: _ "left") CA.int


right
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Screen subj id "right" m Int
    => NodeKey subj id -> C.Getter state m Int
right = getterC (Proxy :: _ "right") CA.int


top
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Screen subj id "top" m Int
    => NodeKey subj id -> C.Getter state m Int
top = getterC (Proxy :: _ "top") CA.int


bottom
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Screen subj id "bottom" m Int
    => NodeKey subj id -> C.Getter state m Int
bottom = getterC (Proxy :: _ "bottom") CA.int


aleft
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Screen subj id "aleft" m Int
    => NodeKey subj id -> C.Getter state m Int
aleft = getterC (Proxy :: _ "aleft") CA.int


aright
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Screen subj id "aright" m Int
    => NodeKey subj id -> C.Getter state m Int
aright = getterC (Proxy :: _ "aright") CA.int


atop
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Screen subj id "atop" m Int
    => NodeKey subj id -> C.Getter state m Int
atop = getterC (Proxy :: _ "atop") CA.int


abottom
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Screen subj id "abottom" m Int
    => NodeKey subj id -> C.Getter state m Int
abottom = getterC (Proxy :: _ "abottom") CA.int


grabKeys
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Screen subj id "grabKeys" m Boolean
    => NodeKey subj id -> C.Getter state m Boolean
grabKeys = getterC (Proxy :: _ "grabKeys") CA.boolean


lockKeys
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Screen subj id "lockKeys" m Boolean
    => NodeKey subj id -> C.Getter state m Boolean
lockKeys = getterC (Proxy :: _ "lockKeys") CA.boolean


hover
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Screen subj id "hover" m (Maybe String)
    => NodeKey subj id -> C.Getter state m (Maybe String)
hover = getterC (Proxy :: _ "hover") (CAC.maybe CA.string)


terminal
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Screen subj id "terminal" m String
    => NodeKey subj id -> C.Getter state m String
terminal = getterC (Proxy :: _ "terminal") CA.string


title
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Screen subj id "title" m String
    => NodeKey subj id -> C.Getter state m String
title = getterC (Proxy :: _ "title") CA.string
