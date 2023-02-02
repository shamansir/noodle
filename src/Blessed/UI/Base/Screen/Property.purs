module Blessed.UI.Base.Screen.Property where

import Prelude

import Prim.Row as R
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Maybe (Maybe(..))

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.Core (Getter, GetterFn, getter) as C
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.BlessedSubj (Screen, Subject)


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
    :: forall subj id sym r' state m a
     . Respresents Screen subj id
    => R.Cons sym a r' PropertiesRow
    => C.GetterFn subj id sym r' PropertiesRow state m a
getter =
    C.getter


focused
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> C.Getter state m (Maybe String)
focused = getter (Proxy :: _ "focused") (CAC.maybe CA.string)


width
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> C.Getter state m Int
width = getter (Proxy :: _ "width") CA.int


height
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> C.Getter state m Int
height = getter (Proxy :: _ "height") CA.int


cols
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> C.Getter state m Int
cols = getter (Proxy :: _ "cols") CA.int


rows
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> C.Getter state m Int
rows = getter (Proxy :: _ "rows") CA.int


left
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> C.Getter state m Int
left = getter (Proxy :: _ "left") CA.int


right
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> C.Getter state m Int
right = getter (Proxy :: _ "right") CA.int


top
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> C.Getter state m Int
top = getter (Proxy :: _ "top") CA.int


bottom
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> C.Getter state m Int
bottom = getter (Proxy :: _ "bottom") CA.int


aleft
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> C.Getter state m Int
aleft = getter (Proxy :: _ "aleft") CA.int


aright
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> C.Getter state m Int
aright = getter (Proxy :: _ "aright") CA.int


atop
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> C.Getter state m Int
atop = getter (Proxy :: _ "atop") CA.int


abottom
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> C.Getter state m Int
abottom = getter (Proxy :: _ "abottom") CA.int


grabKeys
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> C.Getter state m Boolean
grabKeys = getter (Proxy :: _ "grabKeys") CA.boolean


lockKeys
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> C.Getter state m Boolean
lockKeys = getter (Proxy :: _ "lockKeys") CA.boolean


hover
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> C.Getter state m (Maybe String)
hover = getter (Proxy :: _ "hover") (CAC.maybe CA.string)


terminal
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> C.Getter state m String
terminal = getter (Proxy :: _ "terminal") CA.string


title
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Screen subj id
    => NodeKey subj id -> C.Getter state m String
title = getter (Proxy :: _ "title") CA.string
