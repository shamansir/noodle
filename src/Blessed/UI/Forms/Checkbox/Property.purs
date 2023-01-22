module Blessed.UI.Forms.Checkbox.Property where

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
    -- program ::
    -- tput ::
    ( text :: String
    , checked :: Boolean
    )


getter :: forall sym r' m a. R.Cons sym a r' PropertiesRow => C.GetterFn sym r' PropertiesRow m a
getter =
    C.getter


text :: forall m. C.NodeId -> C.Getter m String
text = getter (Proxy :: _ "text") CA.string


checked :: forall m. C.NodeId -> C.Getter m Boolean
checked = getter (Proxy :: _ "checked") CA.boolean
