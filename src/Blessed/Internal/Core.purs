module Blessed.Internal.Core where

-- export BlessedOp from here

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console

import Data.Either (Either)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array ((:))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as Map

import Data.Symbol (reflectSymbol, class IsSymbol)
import Type.Proxy (Proxy(..))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.BlessedOp as I
import Blessed.Internal.Command as I
import Blessed.Internal.JsApi as I
import Blessed.Internal.Codec as Codec
import Data.Identity (Identity)


type NodeId = I.NodeId


data Attribute :: Row Type -> Type -> Type
data Attribute (r :: Row Type) e
    = Property String Json
    | Handler e (NodeId -> Json -> I.BlessedOp Effect)


data OnlyProperty (r :: Row Type)
    = OnlyProperty String Json



instance Functor (Attribute r) where
    map f (Handler e op) = Handler (f e) op
    map _ (Property str json) = Property str json


type Blessed e = I.SNode


-- see Halogen.Svg.Elements + Halogen.Svg.Properties
type Node (r :: Row Type) e = Events e => Array (Attribute r e) -> Array (Blessed CoreEvent) -> Blessed CoreEvent
type NodeAnd (r :: Row Type) e = Array (Attribute r e) -> Array (Blessed CoreEvent) -> (NodeId -> I.BlessedOp Effect) -> Blessed CoreEvent
type Leaf (r :: Row Type) e = Array (Attribute r e) -> Blessed CoreEvent
type LeafAnd (r :: Row Type) e = Array (Attribute r e) ->  (NodeId -> I.BlessedOp Effect) -> Blessed CoreEvent
type Handler (r :: Row Type) e = (NodeId -> Json -> I.BlessedOp Effect) -> Attribute r e


splitAttributes :: forall r e. Events e => Array (Attribute r e) -> Array I.SProp /\ Array I.SHandler
splitAttributes props = Array.catMaybes (lockSProp <$> props) /\ Array.catMaybes (lockSHandler <$> props)
    where
        lockSProp (Property str json) = Just $ I.SProp str json
        lockSProp _ = Nothing
        lockSHandler (Handler e op) =
            case convert e of
                eventId /\ arguments -> Just $ I.makeHandler (I.EventId eventId) arguments op
        lockSHandler _ = Nothing


-- FIXME: no `Cons` check here, but only above
property :: forall (sym :: Symbol) (r :: Row Type) a e. IsSymbol sym => EncodeJson a => Proxy sym -> a -> Attribute r e
property sym = Property (reflectSymbol sym) <<< encodeJson


onlyProperty :: forall (sym :: Symbol) (r :: Row Type) a. IsSymbol sym => EncodeJson a => Proxy sym -> a -> OnlyProperty r
onlyProperty sym = OnlyProperty (reflectSymbol sym) <<< encodeJson


handler :: forall r e. Events e => e -> Handler r e
handler = Handler


node :: forall r e. I.Kind -> String -> Node r e
node kind name attrs children =
    I.SNode kind (I.NodeId name) sprops children handlers
    where sprops /\ handlers = splitAttributes attrs


nodeAnd :: forall r e. Events e => I.Kind -> String -> NodeAnd r e
nodeAnd kind name attrs children fn =
    I.SNode kind (I.NodeId name) sprops children (I.makeHandler (I.EventId initialId) initalArgs (\id _ -> fn id) : handlers)
    where
        sprops /\ handlers = splitAttributes attrs
        initialId /\ initalArgs = convert (initial :: e)


data CoreEvent =
    CoreEvent


class Events e where
    initial :: e
    convert :: e -> String /\ Array Json
    toCore :: e -> CoreEvent
    fromCore :: CoreEvent -> Maybe e
    -- extract :: e -> Json -> Json


instance Events CoreEvent where
    initial = CoreEvent
    convert _ = "Core" /\ []
    toCore = identity
    fromCore = Just
    -- extract _ = identity


instance EncodeJson (OnlyProperty r) where
    encodeJson (OnlyProperty name value)
        = CA.encode Codec.propertyRecCodec { name, value }


encode :: forall e. Blessed e -> I.BlessedEnc
encode = Codec.encode