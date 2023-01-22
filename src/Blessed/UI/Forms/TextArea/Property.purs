module Blessed.UI.Forms.TextArea.Property where

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
    ( value :: String
    )


getter :: forall sym r' m a. R.Cons sym a r' PropertiesRow => C.GetterFn sym r' PropertiesRow m a
getter =
    C.getter


value :: forall m. C.NodeId -> C.Getter m String
value = getter (Proxy :: _ "value") CA.string
