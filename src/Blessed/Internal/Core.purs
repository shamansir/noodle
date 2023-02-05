module Blessed.Internal.Core where

-- export BlessedOp from here

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref (new) as Ref

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

import Blessed.Internal.BlessedOp as Op
import Blessed.Internal.BlessedSubj as K
import Blessed.Internal.Command as C
import Blessed.Internal.NodeKey (NodeKey)
import Blessed.Internal.NodeKey as NK
import Blessed.Internal.JsApi as I
import Blessed.Internal.Codec as Codec
import Blessed.Internal.Emitter
import Blessed.Internal.Foreign (encode) as Foreign



data Attribute :: K.Subject -> Symbol -> Row Type -> Type -> Type -> Type
data Attribute (subj :: K.Subject) (id :: Symbol) (r :: Row Type) state e
    = Option String Json
    | Handler e (NodeKey subj id -> I.EventJson -> Op.BlessedOp state Effect)


data SoleOption (r :: Row Type)
    = SoleOption String Json



instance Functor (Attribute subj id r state) where
    map f (Handler e op) = Handler (f e) op
    map _ (Option str json) = Option str json


-- type Blessed state e = Ref state -> I.SNode state
type Blessed state e = I.SNode state


-- see Halogen.Svg.Elements + Halogen.Svg.Properties
type Node (subj :: K.Subject) (id :: Symbol) (r :: Row Type) state e = Events e => Array (Attribute subj id r state e) -> Array (Blessed state CoreEvent) -> Blessed state CoreEvent
type NodeAnd (subj :: K.Subject) (id :: Symbol) (r :: Row Type) state e = Array (Attribute subj id r state e) -> Array (Blessed state CoreEvent) -> (NodeKey subj id -> Op.BlessedOp state Effect) -> Blessed state CoreEvent
type Leaf (subj :: K.Subject) (id :: Symbol) (r :: Row Type) state e = Array (Attribute subj id r state e) -> Blessed state CoreEvent
type LeafAnd (subj :: K.Subject) (id :: Symbol) (r :: Row Type) state e = Array (Attribute subj id r state e) ->  (NodeKey subj id -> Op.BlessedOp state Effect) -> Blessed state CoreEvent
type Handler (subj :: K.Subject) (id :: Symbol) (r :: Row Type) state e = (NodeKey subj id -> I.EventJson -> Op.BlessedOp state Effect) -> Attribute subj id r state e


splitAttributes :: forall subj id r state e. K.IsSubject subj => IsSymbol id => Events e => Array (Attribute subj id r state e) -> Array I.SProp /\ Array (I.SHandler state)
splitAttributes props = Array.catMaybes (lockSProp <$> props) /\ Array.catMaybes (lockSHandler <$> props)
    where
        lockSProp (Option str json) = Just $ I.SProp str json
        lockSProp _ = Nothing
        lockSHandler (Handler e op) =
            case convert e of
                eventId /\ arguments -> Just $ Op.makeHandler (NK.make (Proxy :: _ subj) (Proxy :: _ id)) (I.EventId eventId) arguments op
        lockSHandler _ = Nothing


-- FIXME: no `Cons` check here, but only above
option :: forall (subj :: K.Subject) (id :: Symbol) (sym :: Symbol) (r :: Row Type) state a e. IsSymbol sym => EncodeJson a => Proxy sym -> a -> Attribute subj id r state e
option sym = Option (reflectSymbol sym) <<< encodeJson


onlyOption :: forall (sym :: Symbol) (r :: Row Type) a. IsSymbol sym => EncodeJson a => Proxy sym -> a -> SoleOption r
onlyOption sym = SoleOption (reflectSymbol sym) <<< encodeJson


handler :: forall subj id r state e. Fires subj e => e -> Handler subj id r state e
handler = Handler


on :: forall subj id r state e. Fires subj e => e -> Handler subj id r state e
on = handler


type Getter state m a = Op.BlessedOpJsonGet state m a


type GetterFn :: forall k. K.Subject -> Symbol -> Symbol -> k -> Row Type -> Type -> (Type -> Type) -> Type -> Type
type GetterFn (subj :: K.Subject) (id :: Symbol) (sym :: Symbol) r' (r :: Row Type) state (m :: Type -> Type) a =
    IsSymbol sym => Proxy sym -> CA.JsonCodec a -> NodeKey subj id -> Getter state m a


type GetterFn' :: forall k. K.Subject -> Symbol -> Symbol -> k -> Row Type -> Type -> (Type -> Type) -> Type -> Type
type GetterFn' (subj :: K.Subject) (id :: Symbol) (sym :: Symbol) r' (r :: Row Type) state (m :: Type -> Type) a =
    IsSymbol sym => EncodeJson a => Proxy sym -> NodeKey subj id -> Getter state m a


getter :: forall subj id sym r' r state m a. K.IsSubject subj => IsSymbol id => GetterFn subj id sym r' r state m a
getter sym codec nodeKey =
    Op.performGet codec (NK.rawify nodeKey) $ C.get $ reflectSymbol sym


getter' :: forall subj id sym r' r state m a. K.IsSubject subj => IsSymbol id => DecodeJson a => GetterFn' subj id sym r' r state m a
getter' sym nodeKey =
    Op.performGet' (NK.rawify nodeKey) $ C.get $ reflectSymbol sym


method ∷ forall subj id state (m ∷ Type -> Type). K.IsSubject subj => IsSymbol id => NodeKey subj id → String → Array Json → Op.BlessedOp state m
method nodeKey name args =
    Op.perform (NK.rawify nodeKey) $ C.call name args


instance EncodeJson (SoleOption r) where
    encodeJson (SoleOption name value)
        = CA.encode Codec.propJson { name, value }


encode :: forall state e. Ref state -> Blessed state e -> I.BlessedEnc
encode = Foreign.encode


node :: forall subj id state r e. K.IsSubject subj => IsSymbol id => NodeKey subj id -> Node subj id state r e
node nodeKey attrs children =
    I.SNode (NK.rawify nodeKey) sprops children handlers
    where sprops /\ handlers = splitAttributes attrs


nodeAnd :: forall subj id state r e. K.IsSubject subj => IsSymbol id => Events e => NodeKey subj id -> NodeAnd subj id state r e
nodeAnd nodeKey attrs children fn =
    I.SNode (NK.rawify nodeKey) sprops children (Op.makeHandler nodeKey (I.EventId initialId) initalArgs (\id _ -> fn id) : handlers)
    where
        sprops /\ handlers = splitAttributes attrs
        initialId /\ initalArgs = convert (initial :: e)


run :: forall state e. state -> Blessed state e -> Effect Unit
run state blessed =
    runAnd state blessed $ pure unit


runAnd :: forall state e. state -> Blessed state e -> Op.BlessedOp state Effect -> Effect Unit
runAnd state blessed op = do
    stateRef <- Ref.new state
    liftEffect $ Op.execute_ $ Foreign.encode stateRef blessed
    Op.runM' stateRef op