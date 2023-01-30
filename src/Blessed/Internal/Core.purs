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
import Blessed.Internal.BlessedSubj as K
import Blessed.Internal.Command as C
import Blessed.Internal.NodeKey (NodeKey)
import Blessed.Internal.NodeKey as NK
import Blessed.Internal.JsApi as I
import Blessed.Internal.Codec as Codec
import Blessed.Internal.Emitter



data Attribute :: K.Subject -> Symbol -> Row Type -> Type -> Type
data Attribute (subj :: K.Subject) (id :: Symbol) (r :: Row Type) e
    = Option String Json
    | Handler e (NodeKey subj id -> Json -> Op.BlessedOp Effect)


data SoleOption (r :: Row Type)
    = SoleOption String Json



instance Functor (Attribute subj id r) where
    map f (Handler e op) = Handler (f e) op
    map _ (Option str json) = Option str json


type Blessed e = I.SNode


-- see Halogen.Svg.Elements + Halogen.Svg.Properties
type Node (subj :: K.Subject) (id :: Symbol) (r :: Row Type) e = Events e => Array (Attribute subj id r e) -> Array (Blessed CoreEvent) -> Blessed CoreEvent
type NodeAnd (subj :: K.Subject) (id :: Symbol) (r :: Row Type) e = Array (Attribute subj id r e) -> Array (Blessed CoreEvent) -> (NodeKey subj id -> Op.BlessedOp Effect) -> Blessed CoreEvent
type Leaf (subj :: K.Subject) (id :: Symbol) (r :: Row Type) e = Array (Attribute subj id r e) -> Blessed CoreEvent
type LeafAnd (subj :: K.Subject) (id :: Symbol) (r :: Row Type) e = Array (Attribute subj id r e) ->  (NodeKey subj id -> Op.BlessedOp Effect) -> Blessed CoreEvent
type Handler (subj :: K.Subject) (id :: Symbol) (r :: Row Type) e = (NodeKey subj id -> Json -> Op.BlessedOp Effect) -> Attribute subj id r e


splitAttributes :: forall subj id r e. K.IsSubject subj => IsSymbol id => Events e => Array (Attribute subj id r e) -> Array I.SProp /\ Array I.SHandler
splitAttributes props = Array.catMaybes (lockSProp <$> props) /\ Array.catMaybes (lockSHandler <$> props)
    where
        lockSProp (Option str json) = Just $ I.SProp str json
        lockSProp _ = Nothing
        lockSHandler (Handler e op) =
            case convert e of
                eventId /\ arguments -> Just $ Op.makeHandler (NK.make (Proxy :: _ subj) (Proxy :: _ id)) (I.EventId eventId) arguments op
        lockSHandler _ = Nothing


-- FIXME: no `Cons` check here, but only above
option :: forall (subj :: K.Subject) (id :: Symbol) (sym :: Symbol) (r :: Row Type) a e. IsSymbol sym => EncodeJson a => Proxy sym -> a -> Attribute subj id r e
option sym = Option (reflectSymbol sym) <<< encodeJson


onlyOption :: forall (sym :: Symbol) (r :: Row Type) a. IsSymbol sym => EncodeJson a => Proxy sym -> a -> SoleOption r
onlyOption sym = SoleOption (reflectSymbol sym) <<< encodeJson


handler :: forall subj id r e. Events e => e -> Handler subj id r e
handler = Handler


type Getter m a = Op.BlessedOpG m a


type GetterFn :: forall k. K.Subject -> Symbol -> Symbol -> k -> Row Type -> (Type -> Type) -> Type -> Type
type GetterFn (subj :: K.Subject) (id :: Symbol) (sym :: Symbol) r' (r :: Row Type) (m :: Type -> Type) a =
    IsSymbol sym => Proxy sym -> CA.JsonCodec a -> NodeKey subj id -> Getter m a


type GetterFn' :: forall k. K.Subject -> Symbol -> Symbol -> k -> Row Type -> (Type -> Type) -> Type -> Type
type GetterFn' (subj :: K.Subject) (id :: Symbol) (sym :: Symbol) r' (r :: Row Type) (m :: Type -> Type) a =
    IsSymbol sym => EncodeJson a => Proxy sym -> NodeKey subj id -> Getter m a


getter :: forall subj id sym r' r m a. K.IsSubject subj => IsSymbol id => GetterFn subj id sym r' r m a
getter sym codec nodeKey =
    Op.performGet codec (NK.rawify nodeKey) $ C.get $ reflectSymbol sym


getter' :: forall subj id sym r' r m a. K.IsSubject subj => IsSymbol id => DecodeJson a => GetterFn' subj id sym r' r m a
getter' sym nodeKey =
    Op.performGet' (NK.rawify nodeKey) $ C.get $ reflectSymbol sym


method ∷ forall subj id (m ∷ Type -> Type). K.IsSubject subj => IsSymbol id => NodeKey subj id → String → Array Json → Op.BlessedOp m
method nodeKey name args =
    Op.perform (NK.rawify nodeKey) $ C.call name args


instance EncodeJson (SoleOption r) where
    encodeJson (SoleOption name value)
        = CA.encode Codec.optionRecCodec { name, value }


encode :: forall e. Blessed e -> I.BlessedEnc
encode = Codec.encode


node :: forall subj id r e. K.IsSubject subj => IsSymbol id => NodeKey subj id -> Node subj id r e
node nodeKey attrs children =
    I.SNode (NK.rawify nodeKey) sprops children handlers
    where sprops /\ handlers = splitAttributes attrs


nodeAnd :: forall subj id r e. K.IsSubject subj => IsSymbol id => Events e => NodeKey subj id -> NodeAnd subj id r e
nodeAnd nodeKey attrs children fn =
    I.SNode (NK.rawify nodeKey) sprops children (Op.makeHandler nodeKey (I.EventId initialId) initalArgs (\id _ -> fn id) : handlers)
    where
        sprops /\ handlers = splitAttributes attrs
        initialId /\ initalArgs = convert (initial :: e)