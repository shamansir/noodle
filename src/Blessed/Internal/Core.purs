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
import Blessed.Internal.Emitter (class Fires, class Events, CoreEvent)
import Blessed.Internal.Emitter (initial, split, typeOf, toCore) as E
import Blessed.Internal.Foreign (encode, encode', encodeHandler, HandlerIndex(..)) as Foreign



type InitFn subj id state = (NodeKey subj id -> Op.BlessedOp state Effect)
type HandlerFn subj id state = (NodeKey subj id -> I.EventJson -> Op.BlessedOp state Effect)


data Attribute :: K.Subject -> Symbol -> Row Type -> Type -> Type -> Type
data Attribute (subj :: K.Subject) (id :: Symbol) (r :: Row Type) state e
    = Option String Json
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
type Node (subj :: K.Subject) (id :: Symbol) (r :: Row Type) state = Array (Attribute subj id r state CoreEvent) -> Array (Blessed state CoreEvent) -> Blessed state CoreEvent
type NodeAnd (subj :: K.Subject) (id :: Symbol) (r :: Row Type) state = Array (Attribute subj id r state CoreEvent) -> Array (Blessed state CoreEvent) -> InitFn subj id state -> Blessed state CoreEvent
type Leaf (subj :: K.Subject) (id :: Symbol) (r :: Row Type) state = Array (Attribute subj id r state CoreEvent) -> Blessed state CoreEvent
type LeafAnd (subj :: K.Subject) (id :: Symbol) (r :: Row Type) state = Array (Attribute subj id r state CoreEvent) -> InitFn subj id state -> Blessed state CoreEvent
type Handler (subj :: K.Subject) (id :: Symbol) (r :: Row Type) state = HandlerFn subj id state -> Attribute subj id r state CoreEvent


splitAttributes :: forall subj id r state e. K.IsSubject subj => IsSymbol id => Events e => Array (Attribute subj id r state e) -> Array I.SProp /\ Array (I.SHandler state)
splitAttributes props = Array.catMaybes (lockSProp <$> props) /\ Array.concat (lockSHandler <$> props)
    where
        nodeKey = NK.make (Proxy :: _ subj) (Proxy :: _ id)
        lockSProp (Option str json) = Just $ I.SProp str json
        lockSProp (OptionWithHandlers str json _) = Just $ I.SProp str json
        lockSProp _ = Nothing
        lockSHandler (Handler e op) =
            case E.split e of
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


-- handler :: forall subj id r state e. Fires subj e => e -> Handler subj id r state CoreEvent
-- handler e handler = E.toCore <$> Handler e handler


handler :: forall subj id r state e. Fires subj e => e -> Handler subj id r state
handler e fn = E.toCore <$> Handler e fn


-- on :: forall subj id r state e. Fires subj e => e -> Handler subj id r state CoreEvent
-- on = handler


on :: forall subj id r state e. Fires subj e => e -> Handler subj id r state
on = handler


type Getter state m a = Op.BlessedOpGet state m a
type Setter state m a = Op.BlessedOpSet state m


-- FIXME: simplify the types and the chain of classes (lenses?)


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


class Sets :: K.Subject -> K.Subject -> Symbol -> Symbol -> (Type -> Type) -> Type -> Constraint
class (K.Extends parent subj, K.IsSubject subj, IsSymbol id, IsSymbol prop, Op.Sets m a) <= Sets parent subj id prop m a
instance (K.Extends parent subj, K.IsSubject subj, IsSymbol id, IsSymbol prop, Op.Sets m a) => Sets parent subj id prop m a


class Sets2 :: K.Subject -> K.Subject -> Symbol -> Symbol -> Symbol -> (Type -> Type) -> Type -> Constraint
class (K.Extends parent subj, K.IsSubject subj, IsSymbol id, IsSymbol propA, IsSymbol propB, Op.Sets m a) <= Sets2 parent subj id propA propB m a
instance (K.Extends parent subj, K.IsSubject subj, IsSymbol id, IsSymbol propA, IsSymbol propB, Op.Sets m a) => Sets2 parent subj id propA propB m a


class SetsC :: forall k. K.Subject -> K.Subject -> Symbol -> Symbol -> (Type -> Type) -> k -> Constraint
class (K.Extends parent subj, K.IsSubject subj, IsSymbol id, IsSymbol prop, Op.SetsC m a) <= SetsC parent subj id prop m a
instance (K.Extends parent subj, K.IsSubject subj,  IsSymbol prop, IsSymbol id, Op.SetsC m a) => SetsC parent subj id prop m a


class SetsC2 :: forall k. K.Subject -> K.Subject -> Symbol -> Symbol -> Symbol -> (Type -> Type) -> k -> Constraint
class (K.Extends parent subj, K.IsSubject subj, IsSymbol id, IsSymbol propA, IsSymbol propB, Op.SetsC m a) <= SetsC2 parent subj id propA propB m a
instance (K.Extends parent subj, K.IsSubject subj, IsSymbol id, IsSymbol propA, IsSymbol propB, Op.SetsC m a) => SetsC2 parent subj id propA propB m a


-- FIXME: simplify the types and the chain of classes (lenses?)


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


type SetterFn (subj :: K.Subject) (id :: Symbol) (prop :: Symbol) state (m :: Type -> Type) a =
    Proxy prop -> a -> NodeKey subj id -> Setter state m a


type SetterFn2 (subj :: K.Subject) (id :: Symbol) (propA :: Symbol) (propB :: Symbol) state (m :: Type -> Type) a =
    Proxy propA -> Proxy propB -> a -> NodeKey subj id -> Setter state m a


type SetterFnC (subj :: K.Subject) (id :: Symbol) (prop :: Symbol) state (m :: Type -> Type) a =
    Proxy prop -> CA.JsonCodec a -> a -> NodeKey subj id -> Setter state m a


type SetterFnC2 (subj :: K.Subject) (id :: Symbol) (propA :: Symbol) (propB :: Symbol) state (m :: Type -> Type) a =
    Proxy propA -> Proxy propB -> CA.JsonCodec a -> a -> NodeKey subj id -> Setter state m a


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


subscription ∷ forall subj id state (m ∷ Type -> Type) e. K.IsSubject subj => IsSymbol id => Fires subj e => NodeKey subj id → e → HandlerFn subj id state -> Op.BlessedOp state m
subscription nodeKey event op =
    Op.getStateRef >>= \stateRef ->
        case E.split event of
            eventId /\ arguments ->
                Op.perform (NK.rawify nodeKey)
                    $ Cmd.sub (E.typeOf eventId) arguments
                    $ Foreign.encodeHandler stateRef (NK.rawify nodeKey) (Foreign.HandlerIndex 0)
                    $ Op.makeHandler nodeKey eventId arguments op


on' :: forall subj id state (m ∷ Type -> Type) e. K.IsSubject subj => IsSymbol id => Fires subj e => e → HandlerFn subj id state -> NodeKey subj id → Op.BlessedOp state m
on' e h key = subscription key e h


setter ∷ forall parent subj id prop state (m ∷ Type -> Type) a. Sets parent subj id prop m a => Proxy parent -> SetterFn subj id prop state m a
setter _ prop cvalue nodeKey =
    Op.perform (NK.rawify nodeKey) $ Cmd.set (reflectSymbol prop) $ encodeJson cvalue


setter2 ∷ forall parent subj id propA propB state (m ∷ Type -> Type) a. Sets2 parent subj id propA propB m a => Proxy parent -> SetterFn2 subj id propA propB state m a
setter2 _ propA propB cvalue nodeKey =
    Op.perform (NK.rawify nodeKey) $ Cmd.setP [ reflectSymbol propA, reflectSymbol propB ] $ encodeJson cvalue


setterC ∷ forall parent subj id prop state (m ∷ Type -> Type) a. SetsC parent subj id prop m a => Proxy parent -> SetterFnC subj id prop state m a
setterC _ prop codec cvalue nodeKey =
    Op.perform (NK.rawify nodeKey) $ Cmd.set (reflectSymbol prop) $ CA.encode codec cvalue


setterC2 ∷ forall parent subj id propA propB state (m ∷ Type -> Type) a. SetsC2 parent subj id propA propB m a => Proxy parent -> SetterFnC2 subj id propA propB state m a
setterC2 _ propA propB codec cvalue nodeKey =
    Op.perform (NK.rawify nodeKey) $ Cmd.setP [ reflectSymbol propA, reflectSymbol propB ] $ CA.encode codec cvalue


instance EncodeJson (SoleOption r) where
    encodeJson (SoleOption name value)
        = CA.encode Codec.propJson { name, value }


encode :: forall state e. Ref state -> Blessed state e -> I.BlessedEnc
encode = Foreign.encode


node :: forall subj id state r. K.IsSubject subj => IsSymbol id => NodeKey subj id -> Node subj id state r
node nodeKey attrs children =
    I.SNode (NK.rawify nodeKey) sprops children handlers
    where sprops /\ handlers = splitAttributes attrs


nodeAnd :: forall subj id state r e. K.IsSubject subj => IsSymbol id => Events e => Proxy e -> NodeKey subj id -> NodeAnd subj id state r
nodeAnd _ nodeKey attrs children fn =
    I.SNode (NK.rawify nodeKey) sprops children (Op.makeHandler nodeKey initialId initalArgs (\id _ -> fn id) : handlers)
    where
        sprops /\ handlers = splitAttributes attrs
        initialId /\ initalArgs = E.split (E.initial :: e)


run :: forall state e. state -> Blessed state e -> Effect Unit
run state blessed =
    runAnd state blessed $ pure unit


runAnd :: forall state e. state -> Blessed state e -> Op.BlessedOp state Effect -> Effect Unit
runAnd state blessed op = do
    stateRef <- Ref.new state
    liftEffect $ Op.execute_ $ Foreign.encode stateRef blessed
    Op.runM' stateRef op