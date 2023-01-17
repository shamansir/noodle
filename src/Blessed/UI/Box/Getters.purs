module Blessed.UI.Box.Getters where

import Prelude

import Prim.Row as R
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)

import Blessed.Core.Style (Style)
import Blessed.Core.Border (Border)

import Data.Codec.Argonaut as CA

import Blessed.Internal.Core (NodeId) as C
import Blessed.Internal.BlessedOp as Op
import Blessed.Internal.Command (get) as C



type PropertiesRow =
    ( top :: String
    , left :: String
    , width :: String
    , height :: String
    , content :: String -- a ?
    , tags :: Boolean
    , draggable :: Boolean
    , hover :: Style
    , style :: Style
    , border :: Border
    )


type Getter m a = Op.BlessedOpG m a


getter :: forall sym r' (r :: Row Type) m a. IsSymbol sym => R.Cons sym a r' PropertiesRow => Proxy sym -> CA.JsonCodec a -> C.NodeId -> Getter m a
getter sym codec nodeId =
    Op.performGet codec nodeId $ C.get $ reflectSymbol sym


top :: forall m. C.NodeId -> Getter m String
top = getter ( Proxy :: _ "top" ) CA.string


left :: forall m. C.NodeId -> Getter m String
left = getter ( Proxy :: _ "left" ) CA.string


width :: forall m. C.NodeId -> Getter m String
width = getter ( Proxy :: _ "width" ) CA.string


height :: forall m. C.NodeId -> Getter m String
height = getter ( Proxy :: _ "height" ) CA.string


content :: forall m. C.NodeId -> Getter m String
content = getter ( Proxy :: _ "content" ) CA.string


tags :: forall m. C.NodeId -> Getter m Boolean
tags = getter ( Proxy :: _ "tags" ) CA.boolean


-- hover :: forall m. C.NodeId -> Getter m Style
-- hover = getter ( Proxy :: _ "hover" ) CA._


-- style :: forall m. C.NodeId -> Getter m Style
-- style = getter ( Proxy :: _ "style" ) CA._


-- border :: forall m. C.NodeId -> Getter m Style
-- border = getter ( Proxy :: _ "border" ) CA._