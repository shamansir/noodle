module Blessed.UI.Lists.FileManager.Property where

import Prelude

import Prim.Row as R
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Maybe (Maybe(..))

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.Core as C
import Blessed.Internal.JsApi (Kind) as C
import Blessed.Internal.BlessedOp as Op
import Blessed.Internal.Command (get) as C
import Blessed.Internal.Codec (kindCodec)


-- newtype Focused = Focused String


type PropertiesRow =
    ( cwd :: String
    ) -- + ListOption


getter :: forall sym r' m a. R.Cons sym a r' PropertiesRow => C.GetterFn sym r' PropertiesRow m a
getter =
    C.getter


cwd :: forall m. C.NodeId -> C.Getter m String
cwd = getter (Proxy :: _ "cwd") CA.string
