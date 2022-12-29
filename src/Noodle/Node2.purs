module Noodle.Node2
  where


import Prelude

import Prim.RowList as RL
import Prim.Row as R
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Eq (class Eq)
-- import Control.Monad.Gen (class MonadGen, chooseInt, unfoldable, sized, resize) as Gen
-- import Data.Char.Gen as CG
import Data.Newtype (class Newtype)
import Data.UUID as UUID

import Data.Array as Array
import Data.Bifunctor (lmap, rmap, bimap)
import Data.Functor.Invariant (class Invariant)
import Data.Traversable as T
import Data.Maybe (Maybe)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.List (List)
import Data.List (length, filter) as List

import Type.Proxy (Proxy(..))

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State as State

import Noodle.Fn2.Process (ProcessM)
import Noodle.Fn2.Process as Process
import Noodle.Fn2.Protocol (Protocol, Tracker, InputChange(..), OutputChange(..))
import Noodle.Fn2.Protocol as Protocol
import Noodle.Fn2.Flow (keysToInputs, keysToOutputs, InputId, OutputId, Input, Output, inputIdToString, outputIdToString, inputId, outputId) as Fn
import Noodle.Fn2 (Fn)
import Noodle.Fn2 (_in, _out, inputsShape, outputsShape, run, run', make, cloneReplace) as Fn

import Record (get, set) as Record
import Record.Extra (keys, class Keys) as Record
import Signal (Signal, (~>))
import Signal as Signal
import Signal.Channel (Channel)
import Signal.Channel as Channel

import Unsafe.Coerce (unsafeCoerce)
import Effect.Console (log) as Console

data Family (s :: Symbol) = Family


newtype UUID = UUID String

derive instance uuidNewtype :: Newtype UUID _


data NodeId f = NodeId (Family f /\ UUID)


instance eqNodeId :: IsSymbol f => Eq (NodeId f) where
    eq = sameIds


infix 4 sameIds as <~~~>

sameIds :: forall fA fB. IsSymbol fA => IsSymbol fB => NodeId fA -> NodeId fB -> Boolean
sameIds (NodeId (familyA /\ UUID hashA)) (NodeId (familyB /\ UUID hashB))
    = (reflectSymbol familyA == reflectSymbol familyB) && (hashA == hashB)


data Node f state (is :: Row Type) (os :: Row Type) m = Node (NodeId f) (Tracker state is os) (Protocol state is os) (Fn state is os m)


-- TODO: implement ToFn
-- TODO: implement HasInputs
-- TODO: implement HasOutputs

{-
class ToFn a state is os where
    toFn :: forall m. a -> Fn state is os m


-- TODO: extend to HasInputs, HasOutputs with getAtInput, getAtOutput, updateInputs, updateOutputs, ...
class (RL.RowToList is g, Record.Keys g) <= HasInputs is g

class (RL.RowToList os g, Record.Keys g) <= HasOutputs os g
-}


make :: forall f state is os m. IsSymbol f => MonadEffect m => Family f -> state -> Record is -> Record os -> ProcessM state is os m Unit -> m (Node f state is os m)
make family state is os process =
    make' family state is os $ Fn.make (reflectSymbol family) process


make' :: forall f state is os m. IsSymbol f => MonadEffect m => Family f -> state -> Record is -> Record os -> Fn state is os m -> m (Node f state is os m)
make' family state is os fn = do
    nodeId <- liftEffect $ nextId family
    tracker /\ protocol <- Protocol.make state is os
    pure $ Node nodeId tracker protocol fn


_in :: Fn.InputId -> String
_in = Fn._in


_out :: Fn.OutputId -> String
_out = Fn._out


{-}
mapM :: forall f state is os m m'. (m ~> m') -> Fn state is os m -> Fn state is os m'
mapM f (Node id protocol processM) = Fn name state is os $ Process.mapMM f processM


imapState :: forall state state' is os m. (state -> state') -> (state' -> state) -> Fn state is os m -> Fn state' is os m
imapState f g (Fn name state is os processM) = Fn name (f state) is os $ Process.imapMState f g processM
-}

{- Running -}

nextId :: forall f. Family f -> Effect (NodeId f)
nextId f = do
    hash <- UUID.generate
    pure $ NodeId $ f /\ UUID hash


run :: forall f state is os m. MonadRec m => MonadEffect m => Node f state is os m -> m Unit
run (Node _ _ protocol fn) = Fn.run' protocol fn


{- Get information about the function -}


_family :: forall f. NodeId f -> Family f
_family (NodeId (family /\ _)) = family


_hash :: forall f. NodeId f -> String
_hash (NodeId (_ /\ UUID hash)) = hash


family :: forall f state is os m. Node f state is os m -> Family f
family (Node nodeId _ _ _) = _family nodeId


familyStr :: forall f state is os m. IsSymbol f => Node f state is os m -> String
familyStr = family >>> reflectSymbol


-- familySym :: forall f state is os m. Node f state is os m -> String
-- familySym (Node nodeId _ _ _) = _family nodeId


hash :: forall f state is os m. Node f state is os m -> String
hash (Node nodeId _ _ _) = _hash nodeId


state :: forall f state is os m. MonadEffect m => Node f state is os m -> m state
state (Node _ _ protocol _) = liftEffect $ protocol.getState unit


inputs :: forall f state is os m. MonadEffect m => Node f state is os m -> m (Record is)
inputs (Node _ _ protocol _) = liftEffect $ Tuple.snd <$> protocol.getInputs unit


outputs :: forall f state is os m. MonadEffect m => Node f state is os m -> m (Record os)
outputs (Node _ _ protocol _) = liftEffect $ Tuple.snd <$> protocol.getOutputs unit


atInput :: forall f i state is' is os m din. MonadEffect m => IsSymbol i => R.Cons i din is' is => Fn.Input i -> Node f state is os m -> m din
atInput i node = inputs node <#> Record.get i


atOutput :: forall f o state is os os' m dout. MonadEffect m => IsSymbol o => R.Cons o dout os' os => Fn.Output o -> Node f state is os m -> m dout
atOutput o node = outputs node <#> Record.get o


atI :: forall f i state is' is os m din. MonadEffect m => IsSymbol i => R.Cons i din is' is => Node f state is os m -> Fn.Input i -> m din
atI = flip atInput


atO :: forall f o state is os os' m dout. MonadEffect m => IsSymbol o => R.Cons o dout os' os => Node f state is os m -> Fn.Output o -> m dout
atO = flip atOutput


-- at' ∷ ∀ (m ∷ Type -> Type) (t364 ∷ Type) (state ∷ Type) (os ∷ Row Type) (is ∷ Row Type) (dout :: Type). Functor m ⇒ Node f state is os m → (Record is -> dout) -> m dout
_at ∷ forall f state is os m din. MonadEffect m ⇒ Node f state is os m → (Record is -> din) -> m din
_at node fn = inputs node <#> fn


at_ ∷ forall f state is os m dout. MonadEffect m ⇒ Node f state is os m → (Record os -> dout) -> m dout
at_ node fn = outputs node <#> fn


get :: forall f state is os m. MonadEffect m => Node f state is os m -> m ( state /\ Record is /\ Record os )
get node = do
    state <- state node
    is <- inputs node
    os <- outputs node
    pure $ state /\ is /\ os


subscribeInput :: forall f state is os m din. (Record is -> din) -> Node f state is os m -> Signal din
subscribeInput fn node = fn <$> subscribeInputs node


subscribeInputs :: forall f state is os m. Node f state is os m -> Signal (Record is)
subscribeInputs (Node _ tracker _ _) = Tuple.snd <$> tracker.inputs


subscribeOutput :: forall f state is os m dout. (Record os -> dout) -> Node f state is os m -> Signal dout
subscribeOutput fn node = fn <$> subscribeOutputs node


subscribeOutputs :: forall f state is os m. Node f state is os m -> Signal (Record os)
subscribeOutputs (Node _ tracker _ _) = Tuple.snd <$> tracker.outputs


subscribeState :: forall f state is os m. Node f state is os m -> Signal state
subscribeState (Node _ tracker _ _) = tracker.state

-- private?
sendOut :: forall f o state is os os' m dout. MonadEffect m => IsSymbol o => R.Cons o dout os' os => Node f state is os m -> Fn.Output o -> dout -> m Unit
sendOut node o = liftEffect <<< sendOut_ node o


-- private?
sendOut_ :: forall f o state is os os' m dout. IsSymbol o => R.Cons o dout os' os => Node f state is os m -> Fn.Output o -> dout -> Effect Unit
sendOut_ (Node _ _ protocol _) output dout =
    protocol.modifyOutputs
        (\curOutputs ->
            (SingleOutput $ Fn.outputId output) /\ Record.set output dout curOutputs
        )


-- private?
sendIn :: forall f i state is is' os m din. MonadEffect m => IsSymbol i => R.Cons i din is' is => Node f state is os m -> Fn.Input i -> din -> m Unit
sendIn node i = liftEffect <<< sendIn_ node i


-- private?
sendIn_ :: forall f i state is is' os m din. IsSymbol i => R.Cons i din is' is => Node f state is os m -> Fn.Input i -> din -> Effect Unit
sendIn_ (Node _ _ protocol _) input din =
    protocol.modifyInputs
        (\curInputs ->
            (SingleInput $ Fn.inputId input) /\ Record.set input din curInputs
        )


-- TODO: subscribeLastInput / subscribeLastOutput

data Link fo fi o i = Link (NodeId fo) (Fn.Output o) (Fn.Input i) (NodeId fi) (Effect Unit)


-- TODO: connect

connect
    :: forall fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' m
     . IsSymbol oA
    => IsSymbol iB
    => R.Cons oA doutA osA' osA
    => R.Cons iB dinB isB' isB
    => MonadEffect m
    => MonadRec m
    => Fn.Output oA
    -> Fn.Input iB
    -> (doutA -> dinB)
    -> Node fA stateA isA osA m
    -> Node fB stateB isB osB m
    -> m (Link fA fB oA iB)
connect
    outputA
    inputB
    convert
    nodeA@(Node nodeAId _ _ _)
    nodeB@(Node nodeBId _ _ _) =
    liftEffect $ do
        flagRef <- Ref.new true
        let
            sendToBIfFlagIsOn dout = do
                -- Monad.whenM
                flagOn <- Ref.read flagRef
                if flagOn then sendIn_ nodeB inputB dout
                else pure unit
        Signal.runSignal $ subscribeOutput (Record.get outputA) nodeA ~> convert ~> sendToBIfFlagIsOn
        pure $ Link nodeAId outputA inputB nodeBId $ Ref.write false flagRef
        -- liftEffect $ Signal.runSignal $ subscribeOutput (Record.get outputA) nodeA ~> convert ~> sendIn_ nodeB inputB
        -- pure unit


disconnect
    :: forall fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' m
     . IsSymbol fA
    => IsSymbol fB
    => IsSymbol oA
    => IsSymbol iB
    => R.Cons oA doutA osA' osA
    => R.Cons iB dinB isB' isB
    => MonadEffect m
    => MonadRec m
    => Show dinB
    => Link fA fB oA iB
    -> Node fA stateA isA osA m
    -> Node fB stateB isB osB m
    -> m Boolean
disconnect (Link nodeAIdL _ _ nodeBIdL doDisconnect) (Node nodeAId _ _ _) (Node nodeBId _ _ _) =
    if (nodeAIdL <~~~> nodeAId) && (nodeBIdL <~~~> nodeBId) then
        liftEffect doDisconnect >>= (const $ pure true)
    else pure false


{-
set :: forall f state is os m. MonadEffect m => ( state /\ Record is /\ Record os ) -> Node f state is os m -> m (Node f state is os m)
set ( state /\ inputs /\ outputs ) node@(Node id protocolS fn) =
    pure node -- FIXME
-}


-- TODO: getAtInput, getAtOutput, updateInputs, updateOutputs, updateState ...



inputsShape :: forall f state (is :: Row Type) os m g. RL.RowToList is g => Record.Keys g => Node f state is os m -> List Fn.InputId
inputsShape (Node _ _ _ fn) = Fn.inputsShape fn


outputsShape :: forall f state is (os :: Row Type) m g. RL.RowToList os g => Record.Keys g => Node f state is os m -> List Fn.OutputId
outputsShape (Node _ _ _ fn) = Fn.outputsShape fn


-- TODO: mapRecord


shape
    :: forall f state (is :: Row Type) (os :: Row Type) m g
     . RL.RowToList is g
    => RL.RowToList os g
    => Record.Keys g
    => Node f state is os m
    -> List Fn.InputId /\ List Fn.OutputId
shape node = inputsShape node /\ outputsShape node


dimensions
    :: forall f state is os m g
     . RL.RowToList is g
    => RL.RowToList os g
    => Record.Keys g
    => Node f state is os m
    -> Int /\ Int
dimensions = shape >>> bimap List.length List.length


dimensionsBy
    :: forall f state is os m g
     . RL.RowToList is g
    => RL.RowToList os g
    => Record.Keys g
    => (Fn.InputId -> Boolean)
    -> (Fn.OutputId -> Boolean)
    -> Node f state is os m
    -> Int /\ Int
dimensionsBy iPred oPred = shape >>> bimap (List.filter iPred >>> List.length) (List.filter oPred >>> List.length)


{-
findInput :: forall i ii o oo state m d. (i -> Boolean) -> Fn state is os m -> Maybe (i /\ ii)
findInput pred (Fn _ inputs _ _) = Array.index inputs =<< Array.findIndex (Tuple.fst >>> pred) inputs


findOutput :: forall i ii o oo state m d. (o -> Boolean) -> Fn i ii o oo state m d -> Maybe (o /\ oo)
findOutput pred (Fn _ _ outputs _) = Array.index outputs =<< Array.findIndex (Tuple.fst >>> pred) outputs
-}


with :: forall f state is os m. MonadEffect m => MonadRec m => Node f state is os m -> ProcessM state is os m Unit -> m Unit
with (Node _ _ protocol fn) process =
    Fn.run' protocol $ Fn.cloneReplace fn process