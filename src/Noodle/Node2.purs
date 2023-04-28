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
import Data.UniqueHash as UniqueHash
import Data.SOrder as SOrder
import Data.SOrder (SOrder, class HasSymbolsOrder)
import Data.Symbol (reifySymbol)

import Data.Array as Array
import Data.Bifunctor (lmap, rmap, bimap)
import Data.Functor.Invariant (class Invariant)
import Data.Traversable as T
import Data.Maybe (Maybe(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.List (List)
import Data.List (length, filter) as List
import Data.UniqueHash (UniqueHash)
import Data.KeyHolder as KH

import Type.Proxy (Proxy(..))

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State as State

import Noodle.Id
import Noodle.Fn2.Process (ProcessM)
import Noodle.Fn2.Process as Process
import Noodle.Fn2.Protocol (Protocol, Tracker, InputChange(..), OutputChange(..))
import Noodle.Fn2.Protocol as Protocol
import Noodle.Fn2 (Fn)
import Noodle.Fn2 (inputsShape, outputsShape, inputsShapeH, outputsShapeH, inputsOrder, outputsOrder, run, run', make, cloneReplace) as Fn

import Record (get, set) as Record
import Record.Extra (keys, class Keys) as Record
import Signal (Signal, (~>))
import Signal as Signal
import Signal.Channel (Channel)
import Signal.Channel as Channel

import Unsafe.Coerce (unsafeCoerce)
import Effect.Console (log) as Console




-- store inputs list in the node (or the family def) itself, create it when we create the node
data Node (f :: Symbol) state (is :: Row Type) (os :: Row Type) m
    = Node
        (NodeId f)
        (Tracker state is os)
        (Protocol state is os)
        (Fn state is os m)


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


make
    :: forall f state (is :: Row Type) (iorder :: SOrder) (os :: Row Type) (oorder :: SOrder) m
     . IsSymbol f
    => HasSymbolsOrder iorder is
    => HasSymbolsOrder oorder os
    => MonadEffect m
    => Family f
    -> state
    -> Proxy iorder
    -> Proxy oorder
    -> Record is
    -> Record os
    -> ProcessM state is os m Unit
    -> m (Node f state is os m)
make family state iorder oorder is os process =
    make' (family' family) state is os $ Fn.make (reflect family) { inputs : iorder, outputs : oorder } process


make'
    :: forall f state (is :: Row Type) (os :: Row Type) m
     . MonadEffect m
    => Family' f
    -> state
    -> Record is
    -> Record os
    -> Fn state is os m
    -> m (Node f state is os m)
make' family state is os fn = do
    nodeId <- liftEffect $ makeNodeId family
    tracker /\ protocol <- Protocol.make state is os
    pure $ Node nodeId tracker protocol fn


id :: forall f state is os m. Node f state is os m -> NodeId f
id (Node id _ _ _) = id


family :: forall f state is os m. Node f state is os m -> Family' f
family = id >>> familyOf


hash :: forall f state is os m. Node f state is os m -> UniqueHash
hash = id >>> hashOf


{-}
mapM :: forall f state is os m m'. (m ~> m') -> Fn state is os m -> Fn state is os m'
mapM f (Node id protocol processM) = Fn name state is os $ Process.mapMM f processM


imapState :: forall state state' is os m. (state -> state') -> (state' -> state) -> Fn state is os m -> Fn state' is os m
imapState f g (Fn name state is os processM) = Fn name (f state) is os $ Process.imapMState f g processM
-}

{- Running -}


run :: forall f state is os m. MonadRec m => MonadEffect m => Node f state is os m -> m Unit
run (Node _ _ protocol fn) = Fn.run' protocol fn


{- Get information about the function -}


state :: forall f state is os m. MonadEffect m => Node f state is os m -> m state
state (Node _ _ protocol _) = liftEffect $ protocol.getState unit


inputs :: forall f state is os m. MonadEffect m => Node f state is os m -> m (Record is)
inputs (Node _ _ protocol _) = liftEffect $ Tuple.snd <$> protocol.getInputs unit


outputs :: forall f state is os m. MonadEffect m => Node f state is os m -> m (Record os)
outputs (Node _ _ protocol _) = liftEffect $ Tuple.snd <$> protocol.getOutputs unit


atInput :: forall f i state is' is os m din. MonadEffect m => HasInput i din is' is => Input i -> Node f state is os m -> m din
atInput i node = inputs node <#> Record.get i


atOutput :: forall f o state is os os' m dout. MonadEffect m => HasOutput o dout os' os => Output o -> Node f state is os m -> m dout
atOutput o node = outputs node <#> Record.get o


atI :: forall f i state is' is os m din. MonadEffect m => HasInput i din is' is => Node f state is os m -> Input i -> m din
atI = flip atInput


atO :: forall f o state is os os' m dout. MonadEffect m => HasOutput o dout os' os => Node f state is os m -> Output o -> m dout
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
sendOut :: forall f o state is os os' m dout. MonadEffect m => HasOutput o dout os' os => Node f state is os m -> Output o -> dout -> m Unit
sendOut node o = liftEffect <<< sendOutE node o


-- private?
sendOutE :: forall f o state is os os' m dout. HasOutput o dout os' os => Node f state is os m -> Output o -> dout -> Effect Unit
sendOutE (Node _ _ protocol _) output dout =
    protocol.modifyOutputs
        (\curOutputs ->
            (SingleOutput $ outputR output) /\ Record.set output dout curOutputs
        )


-- private?
sendOut' :: forall f o state is os os' m dout. MonadEffect m => HasOutput o dout os' os => Node f state is os m -> Output' o -> dout -> m Unit
sendOut' node o = liftEffect <<< sendOutE' node o


-- private?
sendOutE' :: forall f o state is os os' m dout. HasOutput o dout os' os => Node f state is os m -> Output' o -> dout -> Effect Unit
sendOutE' (Node _ _ protocol _) output dout =
    protocol.modifyOutputs
        (\curOutputs ->
            (SingleOutput $ outputR' output) /\ Record.set output dout curOutputs
        )


-- private?
sendIn :: forall f i state is is' os m din. MonadEffect m => HasInput i din is' is => Node f state is os m -> Input i -> din -> m Unit
sendIn node i = liftEffect <<< sendInE node i


-- private?
sendInE :: forall f i state is is' os m din. IsSymbol i => HasInput i din is' is => Node f state is os m -> Input i -> din -> Effect Unit
sendInE (Node _ _ protocol _) input din =
    protocol.modifyInputs
        (\curInputs ->
            (SingleInput $ inputR input) /\ Record.set input din curInputs
        )


sendIn' :: forall f i state is is' os m din. MonadEffect m => HasInput i din is' is => Node f state is os m -> Input' i -> din -> m Unit
sendIn' node i = liftEffect <<< sendInE' node i


-- private?
sendInE' :: forall f i state is is' os m din. IsSymbol i => HasInput i din is' is => Node f state is os m -> Input' i -> din -> Effect Unit
sendInE' (Node _ _ protocol _) input din =
    protocol.modifyInputs
        (\curInputs ->
            (SingleInput $ inputR' input) /\ Record.set input din curInputs
        )


-- TODO: subscribeLastInput / subscribeLastOutput

-- FIXME: just store ID's as strings, close constructor and use constraints on functions to check if nodes/inputs/outputs do exist
data Link fo fi o i = Link (NodeId fo) (Output' o) (Input' i) (NodeId fi) (Effect Unit)


-- FIXME: move to Noodle.Id or Link module
newtype FromId = FromId String
newtype ToId = ToId String
newtype FullId = FullId String


derive newtype instance showFromId :: Show FromId
derive newtype instance showToId :: Show ToId
derive newtype instance showFullId :: Show FullId
derive newtype instance eqFromId :: Eq FromId
derive newtype instance eqToId :: Eq ToId
derive newtype instance eqFullId :: Eq FullId
derive newtype instance ordFromId :: Ord FromId
derive newtype instance ordToId :: Ord ToId
derive newtype instance ordFullId :: Ord FullId


toFromId :: forall fo fi o i. Link fo fi o i -> FromId
toFromId (Link nodeA outA _ _ _) = FromId $ reflect' nodeA <> ">>" <> reflect' outA


toToId :: forall fo fi o i. Link fo fi o i -> ToId
toToId (Link _ _ inB nodeB _) = ToId $ reflect' inB <> "<<" <> reflect' nodeB


toFullId :: forall fo fi o i. Link fo fi o i -> FullId
toFullId (Link nodeA outA inB nodeB _) =
    FullId $ reflect' nodeA <> ">>" <> reflect' outA <> "--" <> reflect' inB <> "<<" <> reflect' nodeB


connect
    :: forall fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' m
     . MonadEffect m
    => MonadRec m
    => HasOutput oA doutA osA' osA
    => HasInput iB dinB isB' isB
    => Output oA
    -> Input iB
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
                if flagOn then sendInE nodeB inputB dout
                else pure unit
        Signal.runSignal $ subscribeOutput (Record.get outputA) nodeA ~> convert ~> sendToBIfFlagIsOn
        pure $ Link nodeAId (output' outputA) (input' inputB) nodeBId $ Ref.write false flagRef


connectAlike
    :: forall fA fB oA iB d stateA stateB isA isB isB' osA osB osA' m
     . MonadEffect m
    => MonadRec m
    => HasOutput oA d osA' osA
    => HasInput iB d isB' isB
    => Output oA
    -> Input iB
    -> Node fA stateA isA osA m
    -> Node fB stateB isB osB m
    -> m (Link fA fB oA iB)
connectAlike
    outputA
    inputB =
    connect outputA inputB identity


connect'
    :: forall fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' m
     . MonadEffect m
    => MonadRec m
    => HasOutput oA doutA osA' osA
    => HasInput iB dinB isB' isB
    => Output' oA
    -> Input' iB
    -> (doutA -> dinB)
    -> Node fA stateA isA osA m
    -> Node fB stateB isB osB m
    -> m (Link fA fB oA iB)
connect'
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
                if flagOn then sendInE' nodeB inputB dout
                else pure unit
        Signal.runSignal $ subscribeOutput (Record.get outputA) nodeA ~> convert ~> sendToBIfFlagIsOn
        pure $ Link nodeAId outputA inputB nodeBId $ Ref.write false flagRef


connectAlike'
    :: forall fA fB oA iB d stateA stateB isA isB isB' osA osB osA' m
     . MonadEffect m
    => MonadRec m
    => HasOutput oA d osA' osA
    => HasInput iB d isB' isB
    => Output' oA
    -> Input' iB
    -> Node fA stateA isA osA m
    -> Node fB stateB isB osB m
    -> m (Link fA fB oA iB)
connectAlike'
    outputA
    inputB =
    connect' outputA inputB identity


disconnect
    :: forall fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' m
     . MonadEffect m
    => MonadRec m
    => IsSymbol fA
    => IsSymbol fB
    => HasOutput oA doutA osA' osA
    => HasInput iB dinB isB' isB
    => Show dinB
    => Link fA fB oA iB
    -> Node fA stateA isA osA m
    -> Node fB stateB isB osB m
    -> m Boolean
disconnect (Link nodeAIdL _ _ nodeBIdL doDisconnect) (Node nodeAId _ _ _) (Node nodeBId _ _ _) =
    if (nodeAIdL == nodeAId) && (nodeBIdL == nodeBId) then
        liftEffect doDisconnect >>= (const $ pure true)
    else pure false



-- withInput :: forall f state is is' os m a. Applicative m => InputR -> (forall i din. Input i -> Node f state is os m -> a) -> Node f state is os m -> a
-- withInput inputR fn node = reifySymbol (reflect' inputR) (\input -> fn input node)


-- withOutput :: forall f state is is' os m a. Applicative m => OutputR -> (forall i din. Output i -> Node f state is os m -> a) -> Node f state is os m -> a
-- withOutput outputR fn node = reifySymbol (reflect' outputR) (\output -> fn output node)


{-
set :: forall f state is os m. MonadEffect m => ( state /\ Record is /\ Record os ) -> Node f state is os m -> m (Node f state is os m)
set ( state /\ inputs /\ outputs ) node@(Node id protocolS fn) =
    pure node -- FIXME
-}


-- TODO: getAtInput, getAtOutput, updateInputs, updateOutputs, updateState ...



inputsShape :: forall f state (is :: Row Type) os m rli. HasInputsAt is rli => Node f state is os m -> List InputR
inputsShape (Node _ _ _ fn) = Fn.inputsShape fn


outputsShape :: forall f state is (os :: Row Type) m rlo. HasOutputsAt os rlo => Node f state is os m -> List OutputR
outputsShape (Node _ _ _ fn) = Fn.outputsShape fn


inputsShapeH :: forall f state (is :: Row Type) os m rli. KH.KeysO rli Input HoldsInput => HasInputsAt is rli => Node f state is os m -> Array HoldsInput
inputsShapeH (Node _ _ _ fn) = Fn.inputsShapeH fn


outputsShapeH :: forall f state is (os :: Row Type) m rlo. KH.KeysO rlo Output HoldsOutput => HasOutputsAt os rlo => Node f state is os m -> Array HoldsOutput
outputsShapeH (Node _ _ _ fn) = Fn.outputsShapeH fn


inputsOrder :: forall f state is os m rli. HasInputsAt is rli => Node f state is os m -> SOrder
inputsOrder (Node _ _ _ fn) = Fn.inputsOrder fn


outputsOrder :: forall f state is os m rlo. HasOutputsAt os rlo => Node f state is os m -> SOrder
outputsOrder (Node _ _ _ fn) = Fn.outputsOrder fn


-- TODO: mapRecord


shape
    :: forall f state (is :: Row Type) (os :: Row Type) m rli rlo
     . HasInputsAt is rli
    => HasOutputsAt os rlo
    => Node f state is os m
    -> List InputR /\ List OutputR
shape node = inputsShape node /\ outputsShape node


shapeH
    :: forall f state (is :: Row Type) (os :: Row Type) m rli rlo
     . KH.KeysO rli Input HoldsInput
    => KH.KeysO rlo Output HoldsOutput
    => HasInputsAt is rli
    => HasOutputsAt os rlo
    => Node f state is os m
    -> Array HoldsInput /\ Array HoldsOutput
shapeH node = inputsShapeH node /\ outputsShapeH node


dimensions
    :: forall f state is os m rli rlo
     . HasInputsAt is rli
    => HasOutputsAt os rlo
    => Node f state is os m
    -> Int /\ Int
dimensions = shape >>> bimap List.length List.length


dimensionsBy
    :: forall f state is os m rli rlo
     . HasInputsAt is rli
    => HasOutputsAt os rlo
    => (InputR -> Boolean)
    -> (OutputR -> Boolean)
    -> Node f state is os m
    -> Int /\ Int
dimensionsBy iPred oPred = shape >>> bimap (List.filter iPred >>> List.length) (List.filter oPred >>> List.length)


-- between two nodes
-- between two nodes, in patch


{- withInput
    :: forall b m
    . Applicative m
    => (  forall f state fs iis is os
        .  HasNodesOf f state fs iis is os m
        => Node.Family f
        -> Family.Def state is os m
        -> Toolkit m
        -> m a
        )
    -> Node.FamilyR
    -> m (Maybe a) -}


{-
findInput :: forall i ii o oo state m d. (i -> Boolean) -> Fn state is os m -> Maybe (i /\ ii)
findInput pred (Fn _ inputs _ _) = Array.index inputs =<< Array.findIndex (Tuple.fst >>> pred) inputs


findOutput :: forall i ii o oo state m d. (o -> Boolean) -> Fn i ii o oo state m d -> Maybe (o /\ oo)
findOutput pred (Fn _ _ outputs _) = Array.index outputs =<< Array.findIndex (Tuple.fst >>> pred) outputs
-}


with :: forall f state is os m. MonadEffect m => MonadRec m => Node f state is os m -> ProcessM state is os m Unit -> m Unit
with (Node _ _ protocol fn) process =
    Fn.run' protocol $ Fn.cloneReplace fn process


newtype HoldsNode' f m = HoldsNode' (forall r. (forall state is os. IsSymbol f => Node f state is os m -> r) -> r)


newtype HoldsNode = HoldsNode (forall r. (forall f state is os m. IsSymbol f => Node f state is os m -> r) -> r)


holdNode :: forall f state is os m. IsSymbol f => Node f state is os m -> HoldsNode
holdNode node = HoldsNode (_ $ node)


holdNode' :: forall f state is os m. IsSymbol f => Node f state is os m -> HoldsNode' f m
holdNode' node = HoldsNode' (_ $ node)


withNode :: forall r. HoldsNode -> (forall f state is os m. IsSymbol f => Node f state is os m -> r) -> r
withNode (HoldsNode f) = f


withNode' :: forall f m r. HoldsNode' f m -> (forall state is os. IsSymbol f => Node f state is os m -> r) -> r
withNode' (HoldsNode' f) = f
