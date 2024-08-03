module Blessed.UI.Lists.List.Property where

import Prelude


import Prim.Row as R
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Maybe (Maybe(..))

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.Core as C
import Blessed.Internal.BlessedSubj (Subject, List, list)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.BlessedOp as Op
import Blessed.Internal.Command (get) as C


-- newtype Focused = Focused String


type PropertiesRow =
    ( selected :: Int
    ) -- + ListOption


getter
    :: forall subj id prop r' state m a
     . C.Gets List subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFn subj id prop state m a
getter =
    C.getter list


getterC
    :: forall subj id prop r' state m a
     . C.GetsC List subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFnC subj id prop state m a
getterC =
    C.getterC list


selected
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC List subj id "selected" m Int
    => NodeKey subj id -> C.Getter state m Int
selected = getterC (Proxy :: _ "selected") CA.int