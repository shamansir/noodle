module Blessed.UI.Base.Node.Property where

import Prelude

import Prim.Row as R
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Maybe (Maybe(..))

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.Core as C
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.BlessedSubj (Node, node, Subject, Subject_)
import Blessed.Internal.Codec (subject_) as Codec


-- newtype Focused = Focused String


type PropertiesRow =
    -- program ::
    -- tput ::
    ( type :: Subject_ -- FIXME: should access `type`
    , options :: Json
    , parent :: Json
    , screen :: Json
    , children :: Array Json
    , data :: Json -- FIXME
    , index :: Int
    )


getter
    :: forall subj id prop r' state m a
     . C.Gets Node subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFn subj id prop state m a
getter =
    C.getter node


getterC
    :: forall subj id prop r' state m a
     . C.GetsC Node subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFnC subj id prop state m a
getterC =
    C.getterC node


type_
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Node subj id "type" m Subject_
    => NodeKey subj id -> C.Getter state m Subject_
type_ = getterC (Proxy :: _ "type") Codec.subject_


options
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Node subj id "options" m Json
    => NodeKey subj id -> C.Getter state m Json
options = getterC (Proxy :: _ "options") CA.json


parent
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Node subj id "parent" m Json
    => NodeKey subj id -> C.Getter state m Json
parent = getterC (Proxy :: _ "parent") CA.json


screen
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Node subj id "screen" m Json
    => NodeKey subj id -> C.Getter state m Json
screen = getterC (Proxy :: _ "screen") CA.json


children
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Node subj id "children" m (Array Json)
    => NodeKey subj id -> C.Getter state m (Array Json)
children = getterC (Proxy :: _ "children") $ CA.array CA.json


data_
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Node subj id "data" m Json
    => NodeKey subj id -> C.Getter state m Json
data_ = getterC (Proxy :: _ "data") CA.json


index
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Node subj id "index" m Int
    => NodeKey subj id -> C.Getter state m Int
index = getterC (Proxy :: _ "index") CA.int
