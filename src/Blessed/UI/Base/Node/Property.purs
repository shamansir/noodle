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
import Blessed.Internal.BlessedSubj (Node, Subject, Subject_)
import Blessed.Internal.Codec (kindCodec)


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
    :: forall subj id sym r' m a
     . Respresents Node subj id
    => R.Cons sym a r' PropertiesRow
    => C.GetterFn subj id sym r' PropertiesRow m a
getter =
    C.getter


type_
    :: forall (subj :: Subject) (id :: Symbol) m
     . Respresents Node subj id
    => NodeKey subj id -> C.Getter m Subject_
type_ = getter (Proxy :: _ "type") kindCodec


options
    :: forall (subj :: Subject) (id :: Symbol) m
     . Respresents Node subj id
    => NodeKey subj id -> C.Getter m Json
options = getter (Proxy :: _ "options") CA.json


parent
    :: forall (subj :: Subject) (id :: Symbol) m
     . Respresents Node subj id
    => NodeKey subj id -> C.Getter m Json
parent = getter (Proxy :: _ "parent") CA.json


screen
    :: forall (subj :: Subject) (id :: Symbol) m
     . Respresents Node subj id
    => NodeKey subj id -> C.Getter m Json
screen = getter (Proxy :: _ "screen") CA.json


children
    :: forall (subj :: Subject) (id :: Symbol) m
     . Respresents Node subj id
    => NodeKey subj id -> C.Getter m (Array Json)
children = getter (Proxy :: _ "children") $ CA.array CA.json


data_
    :: forall (subj :: Subject) (id :: Symbol) m
     . Respresents Node subj id
    => NodeKey subj id -> C.Getter m Json
data_ = getter (Proxy :: _ "data") CA.json


index
    :: forall (subj :: Subject) (id :: Symbol) m
     . Respresents Node subj id
    => NodeKey subj id -> C.Getter m Int
index = getter (Proxy :: _ "index") CA.int
