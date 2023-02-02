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
import Blessed.Internal.BlessedSubj (Subject, Log)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)

-- newtype Focused = Focused String


type PropertiesRow =
    ( scrollback :: Int
    , scrollOnInput :: Boolean
    )


getter
    :: forall subj id sym r' state m a
     . Respresents Log subj id
    => R.Cons sym a r' PropertiesRow
    => C.GetterFn subj id sym r' PropertiesRow state m a
getter =
    C.getter


scrollback
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Log subj id
    => NodeKey subj id -> C.Getter state m Int
scrollback = getter (Proxy :: _ "scrollback") CA.int


scrollOnInput
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents Log subj id
    => NodeKey subj id -> C.Getter state m Boolean
scrollOnInput = getter (Proxy :: _ "scrollOnInput") CA.boolean
