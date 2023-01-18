module Blessed.Internal.Core where

-- export BlessedOp from here

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console

import Prim.Row as R

import Data.Either (Either)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array ((:))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Identity (Identity)

import Data.Symbol (reflectSymbol, class IsSymbol)
import Type.Proxy (Proxy(..))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.BlessedOp as Op
import Blessed.Internal.Command as C
import Blessed.Internal.JsApi as I
import Blessed.Internal.Codec as Codec
import Blessed.Internal.Emitter



type NodeId = I.NodeId


data Attribute :: Row Type -> Type -> Type
data Attribute (r :: Row Type) e
    = Option String Json
    | Handler e (NodeId -> Json -> Op.BlessedOp Effect)


data SoleOption (r :: Row Type)
    = SoleOption String Json



instance Functor (Attribute r) where
    map f (Handler e op) = Handler (f e) op
    map _ (Option str json) = Option str json


type Blessed e = I.SNode


-- see Halogen.Svg.Elements + Halogen.Svg.Properties
type Node (r :: Row Type) e = Events e => Array (Attribute r e) -> Array (Blessed CoreEvent) -> Blessed CoreEvent
type NodeAnd (r :: Row Type) e = Array (Attribute r e) -> Array (Blessed CoreEvent) -> (NodeId -> Op.BlessedOp Effect) -> Blessed CoreEvent
type Leaf (r :: Row Type) e = Array (Attribute r e) -> Blessed CoreEvent
type LeafAnd (r :: Row Type) e = Array (Attribute r e) ->  (NodeId -> Op.BlessedOp Effect) -> Blessed CoreEvent
type Handler (r :: Row Type) e = (NodeId -> Json -> Op.BlessedOp Effect) -> Attribute r e


splitAttributes :: forall r e. Events e => Array (Attribute r e) -> Array I.SProp /\ Array I.SHandler
splitAttributes props = Array.catMaybes (lockSProp <$> props) /\ Array.catMaybes (lockSHandler <$> props)
    where
        lockSProp (Option str json) = Just $ I.SProp str json
        lockSProp _ = Nothing
        lockSHandler (Handler e op) =
            case convert e of
                eventId /\ arguments -> Just $ Op.makeHandler (I.EventId eventId) arguments op
        lockSHandler _ = Nothing


-- FIXME: no `Cons` check here, but only above
option :: forall (sym :: Symbol) (r :: Row Type) a e. IsSymbol sym => EncodeJson a => Proxy sym -> a -> Attribute r e
option sym = Option (reflectSymbol sym) <<< encodeJson


onlyOption :: forall (sym :: Symbol) (r :: Row Type) a. IsSymbol sym => EncodeJson a => Proxy sym -> a -> SoleOption r
onlyOption sym = SoleOption (reflectSymbol sym) <<< encodeJson


handler :: forall r e. Events e => e -> Handler r e
handler = Handler


type Getter m a = Op.BlessedOpG m a


type GetterFn :: forall k. Symbol -> k -> Row Type -> (Type -> Type) -> Type -> Type
type GetterFn (sym :: Symbol) r' (r :: Row Type) (m :: Type -> Type) a =
    IsSymbol sym => Proxy sym -> CA.JsonCodec a -> NodeId -> Getter m a


type GetterFn' :: forall k. Symbol -> k -> Row Type -> (Type -> Type) -> Type -> Type
type GetterFn' (sym :: Symbol) r' (r :: Row Type) (m :: Type -> Type) a =
    IsSymbol sym => EncodeJson a => Proxy sym -> NodeId -> Getter m a


getter :: forall sym r' r m a. GetterFn sym r' r m a
getter sym codec nodeId =
    Op.performGet codec nodeId $ C.get $ reflectSymbol sym


getter' :: forall sym r' r m a. DecodeJson a => GetterFn' sym r' r m a
getter' sym nodeId =
    Op.performGet' nodeId $ C.get $ reflectSymbol sym


method ∷ forall (m ∷ Type -> Type). NodeId → String → Array Json → Op.BlessedOp m
method nodeId name args =
    Op.perform nodeId $ C.call name args


instance EncodeJson (SoleOption r) where
    encodeJson (SoleOption name value)
        = CA.encode Codec.optionRecCodec { name, value }


encode :: forall e. Blessed e -> I.BlessedEnc
encode = Codec.encode


node :: forall r e. I.Kind -> String -> Node r e
node kind name attrs children =
    I.SNode kind (I.NodeId name) sprops children handlers
    where sprops /\ handlers = splitAttributes attrs


nodeAnd :: forall r e. Events e => I.Kind -> String -> NodeAnd r e
nodeAnd kind name attrs children fn =
    I.SNode kind (I.NodeId name) sprops children (Op.makeHandler (I.EventId initialId) initalArgs (\id _ -> fn id) : handlers)
    where
        sprops /\ handlers = splitAttributes attrs
        initialId /\ initalArgs = convert (initial :: e)