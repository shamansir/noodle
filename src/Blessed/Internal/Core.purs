module Blessed.Internal.Core where

-- export BlessedOp from here

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref (new) as Ref

import Prim.Row as R

import Control.Monad.Error.Class (class MonadThrow, throwError)

import Data.Either (Either)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array ((:))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Identity (Identity)
import Data.Foldable (foldr)
import Data.Bifunctor (lmap)

import Data.Symbol (reflectSymbol, class IsSymbol)
import Type.Proxy (Proxy(..))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Codec.Argonaut as CA

import Blessed.Internal.BlessedOp as Op
import Blessed.Internal.BlessedSubj as K
import Blessed.Internal.Command as Cmd
import Blessed.Internal.NodeKey (NodeKey)
import Blessed.Internal.NodeKey as NK
import Blessed.Internal.JsApi as I
import Blessed.Internal.Codec as Codec
import Blessed.Internal.Emitter (class Fires, class Events, EventId, initial, convert, CoreEvent, split)
import Blessed.Internal.Foreign (encode, encode') as Foreign



type InitFn subj id state = (NodeKey subj id -> Op.BlessedOp state Effect)
type HandlerFn subj id state = (NodeKey subj id -> I.EventJson -> Op.BlessedOp state Effect)


data Attribute :: K.Subject -> Symbol -> Row Type -> Type -> Type -> Type
data Attribute (subj :: K.Subject) (id :: Symbol) (r :: Row Type) state e
    = Option String Json
    -- | Handler e (NodeKey subj id) (I.EventJson -> Op.BlessedOp state Effect)
    -- | Handlers (Array (e /\ NodeKey subj id /\ (I.EventJson -> Op.BlessedOp state Effect)))
    | Handler e (HandlerFn subj id state)
    | OptionWithHandlers String Json (Array (e /\ HandlerFn subj id state))


data SoleOption (r :: Row Type)
    = SoleOption String Json


instance Functor (Attribute subj id r state) where
    map _ (Option str json) = Option str json
    map f (Handler e op) = Handler (f e) op
    map f (OptionWithHandlers str json handlersArray) = OptionWithHandlers str json $ lmap f <$> handlersArray


-- type Blessed state e = Ref state -> I.SNode state
type Blessed state e = I.SNode state


-- see Halogen.Svg.Elements + Halogen.Svg.Properties
type Node (subj :: K.Subject) (id :: Symbol) (r :: Row Type) state e = Events e => Array (Attribute subj id r state e) -> Array (Blessed state CoreEvent) -> Blessed state CoreEvent
type NodeAnd (subj :: K.Subject) (id :: Symbol) (r :: Row Type) state e = Array (Attribute subj id r state e) -> Array (Blessed state CoreEvent) -> InitFn subj id state -> Blessed state CoreEvent
type Leaf (subj :: K.Subject) (id :: Symbol) (r :: Row Type) state e = Array (Attribute subj id r state e) -> Blessed state CoreEvent
type LeafAnd (subj :: K.Subject) (id :: Symbol) (r :: Row Type) state e = Array (Attribute subj id r state e) -> InitFn subj id state -> Blessed state CoreEvent
type Handler (subj :: K.Subject) (id :: Symbol) (r :: Row Type) state e = HandlerFn subj id state -> Attribute subj id r state e


splitAttributes :: forall subj id r state e. K.IsSubject subj => IsSymbol id => Events e => Array (Attribute subj id r state e) -> Array I.SProp /\ Array (I.SHandler state)
splitAttributes props = Array.catMaybes (lockSProp <$> props) /\ Array.concat (lockSHandler <$> props)
    where
        nodeKey = NK.make (Proxy :: _ subj) (Proxy :: _ id)
        lockSProp (Option str json) = Just $ I.SProp str json
        lockSProp (OptionWithHandlers str json _) = Just $ I.SProp str json
        lockSProp _ = Nothing
        lockSHandler (Handler e op) =
            case split e of
                eventId /\ arguments -> [ Op.makeHandler nodeKey eventId arguments op ]
        lockSHandler (OptionWithHandlers _ _ handlersArray) =
            Array.concat $ lockSHandler <$> uncurry Handler <$> handlersArray
        lockSHandler _ = []


-- FIXME: no `Cons` check here, but only above
option :: forall (subj :: K.Subject) (id :: Symbol) (sym :: Symbol) (r :: Row Type) state a e. IsSymbol sym => EncodeJson a => Proxy sym -> a -> Attribute subj id r state e
option sym = Option (reflectSymbol sym) <<< encodeJson


optionWithHandlers :: forall (subj :: K.Subject) (id :: Symbol) (sym :: Symbol) (r :: Row Type) state a e. IsSymbol sym => EncodeJson a => Proxy sym -> a -> Array (e /\ HandlerFn subj id state) -> Attribute subj id r state e
optionWithHandlers sym json = OptionWithHandlers (reflectSymbol sym) (encodeJson json)


onlyOption :: forall (sym :: Symbol) (r :: Row Type) a. IsSymbol sym => EncodeJson a => Proxy sym -> a -> SoleOption r
onlyOption sym = SoleOption (reflectSymbol sym) <<< encodeJson


handler :: forall subj id r state e. Fires subj e => e -> Handler subj id r state e
handler = Handler


on :: forall subj id r state e. Fires subj e => e -> Handler subj id r state e
on = handler


type Getter state m a = Op.BlessedOpGet state m a


class Gets :: K.Subject -> K.Subject -> Symbol -> Symbol -> (Type -> Type) -> Type -> Constraint
class (K.Extends parent subj, K.IsSubject subj, IsSymbol id, IsSymbol prop, Op.Gets m a) <= Gets parent subj id prop m a
instance (K.Extends parent subj, K.IsSubject subj, IsSymbol id, IsSymbol prop, Op.Gets m a) => Gets parent subj id prop m a


class Gets2 :: K.Subject -> K.Subject -> Symbol -> Symbol -> Symbol -> (Type -> Type) -> Type -> Constraint
class (K.Extends parent subj, K.IsSubject subj, IsSymbol id, IsSymbol propA, IsSymbol propB, Op.Gets m a) <= Gets2 parent subj id propA propB m a
instance (K.Extends parent subj, K.IsSubject subj, IsSymbol id, IsSymbol propA, IsSymbol propB, Op.Gets m a) => Gets2 parent subj id propA propB m a


class GetsC :: forall k. K.Subject -> K.Subject -> Symbol -> Symbol -> (Type -> Type) -> k -> Constraint
class (K.Extends parent subj, K.IsSubject subj, IsSymbol id, IsSymbol prop, Op.GetsC m a) <= GetsC parent subj id prop m a
instance (K.Extends parent subj, K.IsSubject subj,  IsSymbol prop, IsSymbol id, Op.GetsC m a) => GetsC parent subj id prop m a


class GetsC2 :: forall k. K.Subject -> K.Subject -> Symbol -> Symbol -> Symbol -> (Type -> Type) -> k -> Constraint
class (K.Extends parent subj, K.IsSubject subj, IsSymbol id, IsSymbol propA, IsSymbol propB, Op.GetsC m a) <= GetsC2 parent subj id propA propB m a
instance (K.Extends parent subj, K.IsSubject subj, IsSymbol id, IsSymbol propA, IsSymbol propB, Op.GetsC m a) => GetsC2 parent subj id propA propB m a


-- type GetterFn :: forall k. K.Subject -> K.Subject -> Symbol -> Symbol -> k -> Row Type -> Type -> (Type -> Type) -> Type -> Type
type GetterFn (subj :: K.Subject) (id :: Symbol) (prop :: Symbol) state (m :: Type -> Type) a =
    Proxy prop -> NodeKey subj id -> Getter state m a


type GetterFn2 (subj :: K.Subject) (id :: Symbol) (propA :: Symbol) (propB :: Symbol) state (m :: Type -> Type) a =
    Proxy propA -> Proxy propB -> NodeKey subj id -> Getter state m a


-- type GetterFnC :: forall k. K.Subject -> K.Subject -> Symbol -> Symbol -> k -> Row Type -> Type -> (Type -> Type) -> Type -> Type
type GetterFnC (subj :: K.Subject) (id :: Symbol) (prop :: Symbol) state (m :: Type -> Type) a =
    Proxy prop -> CA.JsonCodec a -> NodeKey subj id -> Getter state m a


type GetterFnC2 (subj :: K.Subject) (id :: Symbol) (propA :: Symbol) (propB :: Symbol) state (m :: Type -> Type) a =
    Proxy propA -> Proxy propB -> CA.JsonCodec a -> NodeKey subj id -> Getter state m a


getter :: forall parent subj id prop state m a. Gets parent subj id prop m a => Proxy parent -> GetterFn subj id prop state m a
getter _ prop nodeKey =
    Op.performGet (NK.rawify nodeKey) $ Cmd.get $ reflectSymbol prop


getterC :: forall parent subj id prop state m a. GetsC parent subj id prop m a => Proxy parent -> GetterFnC subj id prop state m a
getterC _ prop codec nodeKey =
    Op.performGetC codec (NK.rawify nodeKey) $ Cmd.get $ reflectSymbol prop


getter2 :: forall parent subj id propA propB state m a. Gets2 parent subj id propA propB m a => Proxy parent -> GetterFn2 subj id propA propB state m a
getter2 _ propA propB nodeKey =
    Op.performGet (NK.rawify nodeKey) $ Cmd.getP [ reflectSymbol propA, reflectSymbol propB ]


getterC2 :: forall parent subj id propA propB state m a. GetsC2 parent subj id propA propB m a => Proxy parent -> GetterFnC2 subj id propA propB state m a
getterC2 _ propA propB codec nodeKey =
    Op.performGetC codec (NK.rawify nodeKey) $ Cmd.getP [ reflectSymbol propA, reflectSymbol propB ]


method ∷ forall subj id state (m ∷ Type -> Type). K.IsSubject subj => IsSymbol id => NodeKey subj id → String → Array Json → Op.BlessedOp state m
method nodeKey name args =
    Op.perform (NK.rawify nodeKey) $ Cmd.call name args


nmethod ∷ forall subj id state (m ∷ Type -> Type). K.IsSubject subj => IsSymbol id => NodeKey subj id → String → Array (Cmd.NodeOrJson state) → Op.BlessedOp state m
nmethod nodeKey name args =
    Op.getStateRef >>= \stateRef ->
        let
            foldF (Cmd.JsonArg json) (allJsons /\ allHandlers) = Array.snoc allJsons json /\ allHandlers
            foldF (Cmd.NodeArg node) (allJsons /\ allHandlers) =
                case Foreign.encode' stateRef (Just $ NK.rawify nodeKey) node of
                    nodeEnc /\ nodeHandlers -> Array.snoc allJsons (CA.encode Codec.nodeEnc nodeEnc) /\ (allHandlers <> nodeHandlers)
            jsonArgs /\ handlers = foldr foldF ([] /\ []) args
        in Op.perform (NK.rawify nodeKey) $ Cmd.callEx name jsonArgs handlers


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
    I.SNode (NK.rawify nodeKey) sprops children (Op.makeHandler nodeKey initialId initalArgs (\id _ -> fn id) : handlers)
    where
        sprops /\ handlers = splitAttributes attrs
        initialId /\ initalArgs = split (initial :: e)


run :: forall state e. state -> Blessed state e -> Effect Unit
run state blessed =
    runAnd state blessed $ pure unit


runAnd :: forall state e. state -> Blessed state e -> Op.BlessedOp state Effect -> Effect Unit
runAnd state blessed op = do
    stateRef <- Ref.new state
    liftEffect $ Op.execute_ $ Foreign.encode stateRef blessed
    Op.runM' stateRef op