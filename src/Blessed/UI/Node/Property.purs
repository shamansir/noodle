module Blessed.UI.Node.Property where

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
    -- program ::
    -- tput ::
    ( type :: C.Kind -- FIXME: should access `type`
    , options :: Json
    , parent :: Json
    , screen :: Json
    , children :: Array Json
    , data :: Json -- FIXME
    , index :: Int
    )


getter :: forall sym r' m a. R.Cons sym a r' PropertiesRow => C.GetterFn sym r' PropertiesRow m a
getter =
    C.getter


type_ :: forall m. C.NodeId -> C.Getter m C.Kind
type_ = getter (Proxy :: _ "type") kindCodec


options :: forall m. C.NodeId -> C.Getter m Json
options = getter (Proxy :: _ "options") CA.json


parent :: forall m. C.NodeId -> C.Getter m Json
parent = getter (Proxy :: _ "parent") CA.json


screen :: forall m. C.NodeId -> C.Getter m Json
screen = getter (Proxy :: _ "screen") CA.json


children :: forall m. C.NodeId -> C.Getter m (Array Json)
children = getter (Proxy :: _ "children") $ CA.array CA.json


data_ :: forall m. C.NodeId -> C.Getter m Json
data_ = getter (Proxy :: _ "data") CA.json


index :: forall m. C.NodeId -> C.Getter m Int
index = getter (Proxy :: _ "index") CA.int
