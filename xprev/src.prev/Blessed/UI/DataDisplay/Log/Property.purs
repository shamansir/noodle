module Blessed.UI.DataDisplay.Log.Property where


import Prelude

import Prim.Row as R
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Maybe (Maybe(..))

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.Core as C
import Blessed.Internal.BlessedSubj (Subject, Log, log)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)

-- newtype Focused = Focused String


type PropertiesRow =
    ( scrollback :: Int
    , scrollOnInput :: Boolean
    )


getter
    :: forall subj id prop r' state m a
     . C.Gets Log subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFn subj id prop state m a
getter =
    C.getter log


getterC
    :: forall subj id prop r' state m a
     . C.GetsC Log subj id prop m a
    => R.Cons prop a r' PropertiesRow
    => C.GetterFnC subj id prop state m a
getterC =
    C.getterC log


scrollback
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Log subj id "scrollback" m Int
    => NodeKey subj id -> C.Getter state m Int
scrollback = getterC (Proxy :: _ "scrollback") CA.int


scrollOnInput
    :: forall (subj :: Subject) (id :: Symbol) state m
     . C.GetsC Log subj id "scrollOnInput" m Boolean
    => NodeKey subj id -> C.Getter state m Boolean
scrollOnInput = getterC (Proxy :: _ "scrollOnInput") CA.boolean
