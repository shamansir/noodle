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
import Blessed.Internal.BlessedSubj (Subject, List)
import Blessed.Internal.NodeKey (NodeKey, class Respresents)
import Blessed.Internal.BlessedOp as Op
import Blessed.Internal.Command (get) as C
import Blessed.Internal.Codec (kindCodec)


-- newtype Focused = Focused String


type PropertiesRow =
    ( selected :: Int
    ) -- + ListOption


getter
    :: forall subj id sym r' state m a
     . Respresents List subj id
    => R.Cons sym a r' PropertiesRow
    => C.GetterFn subj id sym r' PropertiesRow state m a
getter =
    C.getter


selected
    :: forall (subj :: Subject) (id :: Symbol) state m
     . Respresents List subj id
    => NodeKey subj id -> C.Getter state m Int
selected = getter (Proxy :: _ "selected") CA.int
