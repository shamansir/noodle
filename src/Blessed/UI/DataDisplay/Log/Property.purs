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


-- newtype Focused = Focused String


type PropertiesRow =
    ( scrollback :: Int
    , scrollOnInput :: Boolean
    )


getter :: forall sym r' m a. R.Cons sym a r' PropertiesRow => C.GetterFn sym r' PropertiesRow m a
getter =
    C.getter


scrollback :: forall m. C.NodeId -> C.Getter m Int
scrollback = getter (Proxy :: _ "scrollback") CA.int


scrollOnInput :: forall m. C.NodeId -> C.Getter m Boolean
scrollOnInput = getter (Proxy :: _ "scrollOnInput") CA.boolean
