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
import Data.SProxy (proxify, class Reflect, class Reflect', reflect, reflect')

import Data.Array ((:))
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
import Data.Repr (Repr, class FromRepr, class ToRepr, class FromToReprRow, toRepr, fromRepr, class ReadWriteRepr)

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
import Noodle.Fn2.Protocol (Protocol, Tracker, InputChange(..), OutputChange(..), ChangeFocus)
import Noodle.Fn2.Protocol as Protocol
import Noodle.Fn2 (Fn)
import Noodle.Fn2 (inputsShape, outputsShape, inputsShapeHeld, outputsShapeHeld, inputsOrder, outputsOrder, run, run', make, cloneReplace) as Fn

import Record (get, set) as Record
import Record.Extra (keys, class Keys) as Record
import Signal (Signal, (~>))
import Signal as Signal
import Signal.Channel (Channel)
import Signal.Channel as Channel
import Signal.Extra (runSignal) as SignalX

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


instance IsSymbol f => Eq (Node f state is os m) where
    eq :: Node f state is os m -> Node f state is os m -> Boolean
    eq nodeA nodeB = reflect' (id nodeA) == reflect' (id nodeB)


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


{- makeRun
    :: forall f state (is :: Row Type) (iorder :: SOrder) (os :: Row Type) (oorder :: SOrder) m
     . IsSymbol f
    => HasSymbolsOrder iorder is
    => HasSymbolsOrder oorder os
    => MonadRec m
    => MonadEffect m
    => Family f
    -> state
    -> Proxy iorder
    -> Proxy oorder
    -> Record is
    -> Record os
    -> ProcessM state is os m Unit
    -> m (Node f state is os m)
makeRun family state iorder oorder is os process = do
  node <- make family state iorder oorder is os process
  listenUpdatesAndRun node
  pure node


makeRun'
    :: forall f state (is :: Row Type) (os :: Row Type) m
     . MonadRec m
    => MonadEffect m
    => Family' f
    -> state
    -> Record is
    -> Record os
    -> Fn state is os m
    -> m (Node f state is os m)
makeRun' family state is os fn = do
  node <- make' family state is os fn
  listenUpdatesAndRun node
  pure node -}


-- TODO: private
runOnInputUpdates
    :: forall f state (is :: Row Type) (os :: Row Type) m
    .  MonadRec m
    => MonadEffect m
    => Node f state is os m
    -> m Unit
runOnInputUpdates node =
  SignalX.runSignal $ subscribeInputs node ~> const (run node)


-- TODO: private
runOnStateUpdates
    :: forall f state (is :: Row Type) (os :: Row Type) m
    .  MonadRec m
    => MonadEffect m
    => Node f state is os m
    -> m Unit
runOnStateUpdates node =
  SignalX.runSignal $ subscribeState node ~> const (run node)


--- FIXME: find better name
listenUpdatesAndRun
  :: forall f state (is :: Row Type) (os :: Row Type) m
   . MonadRec m
  => MonadEffect m
  => Node f state is os m
  -> m Unit
listenUpdatesAndRun node = do
  runOnInputUpdates node
  runOnStateUpdates node -- may be running on state updates is not needed
  run node
  -- TODO: FIXME: trigger current update on inputs, so that UI will be informed


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


inputsRow :: forall f state is os m. MonadEffect m => Node f state is os m -> Proxy is
inputsRow _ = Proxy :: _ is


outputsRow :: forall f state is os m. MonadEffect m => Node f state is os m -> Proxy os
outputsRow _ = Proxy :: _ os


atInput :: forall f i state is' is os m din. MonadEffect m => HasInput i din is' is => Input i -> Node f state is os m -> m din
atInput i node = inputs node <#> Record.get (proxify i)


atOutput :: forall f o state is os os' m dout. MonadEffect m => HasOutput o dout os' os => Output o -> Node f state is os m -> m dout
atOutput o node = outputs node <#> Record.get (proxify o)


atI :: forall f i state is' is os m din. MonadEffect m => HasInput i din is' is => Node f state is os m -> Input i -> m din
atI = flip atInput


atO :: forall f o state is os os' m dout. MonadEffect m => HasOutput o dout os' os => Node f state is os m -> Output o -> m dout
atO = flip atOutput

-- TODO: operator
-- at' ∷ ∀ (m ∷ Type -> Type) (t364 ∷ Type) (state ∷ Type) (os ∷ Row Type) (is ∷ Row Type) (dout :: Type). Functor m ⇒ Node f state is os m → (Record is -> dout) -> m dout
_at ∷ forall f state is os m din. MonadEffect m ⇒ Node f state is os m → (Record is -> din) -> m din
_at node fn = inputs node <#> fn


-- TODO: operator
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


subscribeInputs' :: forall f state is os m. Node f state is os m -> Signal (InputChange /\ Record is)
subscribeInputs' (Node _ tracker _ _) = tracker.inputs


subscribeOutput :: forall f state is os m dout. (Record os -> dout) -> Node f state is os m -> Signal dout
subscribeOutput fn node = fn <$> subscribeOutputs node


subscribeOutputs :: forall f state is os m. Node f state is os m -> Signal (Record os)
subscribeOutputs (Node _ tracker _ _) = Tuple.snd <$> tracker.outputs


subscribeOutputs' :: forall f state is os m. Node f state is os m -> Signal (OutputChange /\ Record os)
subscribeOutputs' (Node _ tracker _ _) = tracker.outputs


subscribeState :: forall f state is os m. Node f state is os m -> Signal state
subscribeState (Node _ tracker _ _) = tracker.state


subscribeChanges :: forall f state is os m. Node f state is os m -> Signal (ChangeFocus /\ state /\ Record is /\ Record os)
subscribeChanges (Node _ tracker _ _) = tracker.all


-- private?
sendOut :: forall f o state is os os' m m' dout. MonadEffect m => HasOutput o dout os' os => Node f state is os m' -> Output o -> dout -> m Unit
sendOut node o = liftEffect <<< sendOutE node o


-- private?
sendOutE :: forall f o state is os os' m dout. HasOutput o dout os' os => Node f state is os m -> Output o -> dout -> Effect Unit
sendOutE (Node _ _ protocol _) output dout =
    protocol.modifyOutputs
        (\curOutputs ->
            (SingleOutput $ outputR output) /\ Record.set (proxify output) dout curOutputs
        )


-- private?
sendOut' :: forall f o state is os os' m m' dout. MonadEffect m => HasOutput o dout os' os => Node f state is os m' -> Output' o -> dout -> m Unit
sendOut' node o = liftEffect <<< sendOutE' node o


-- private?
sendOutE' :: forall f o state is os os' m dout. HasOutput o dout os' os => Node f state is os m -> Output' o -> dout -> Effect Unit
sendOutE' (Node _ _ protocol _) output dout =
    protocol.modifyOutputs
        (\curOutputs ->
            (SingleOutput $ outputR' output) /\ Record.set (proxify output) dout curOutputs
        )


-- private?
sendIn :: forall f i state is is' os m m' din. MonadEffect m => HasInput i din is' is => Node f state is os m' -> Input i -> din -> m Unit
sendIn node i = liftEffect <<< sendInE node i


-- private?
sendInE :: forall f i state is is' os m din. IsSymbol i => HasInput i din is' is => Node f state is os m -> Input i -> din -> Effect Unit
sendInE (Node _ _ protocol _) input din =
    protocol.modifyInputs
        (\curInputs ->
            (SingleInput $ inputR input) /\ Record.set (proxify input) din curInputs
        )


sendIn' :: forall f i state is is' os m m' din. MonadEffect m => HasInput i din is' is => Node f state is os m' -> Input' i -> din -> m Unit
sendIn' node i = liftEffect <<< sendInE' node i


-- private?
sendInE' :: forall f i state is is' os m din. IsSymbol i => HasInput i din is' is => Node f state is os m -> Input' i -> din -> Effect Unit
sendInE' (Node _ _ protocol _) input din =
    protocol.modifyInputs
        (\curInputs ->
            (SingleInput $ inputR' input) /\ Record.set (proxify input) din curInputs
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


fromNode :: forall fo fi o i. Link fo fi o i -> NodeId fo
fromNode (Link nodeA _ _ _ _) = nodeA


toNode :: forall fo fi o i. Link fo fi o i -> NodeId fi
toNode (Link _ _ _ nodeB _) = nodeB


toFromId :: forall fo fi o i. Link fo fi o i -> FromId
toFromId (Link nodeA outA _ _ _) = FromId $ reflect' nodeA <> ">>" <> reflect' outA


toToId :: forall fo fi o i. Link fo fi o i -> ToId
toToId (Link _ _ inB nodeB _) = ToId $ reflect' inB <> "<<" <> reflect' nodeB


toFullId :: forall fo fi o i. Link fo fi o i -> FullId
toFullId (Link nodeA outA inB nodeB _) =
    FullId $ reflect' nodeA <> ">>" <> reflect' outA <> "--" <> reflect' inB <> "<<" <> reflect' nodeB


-- TODO: connectByRepr (use Data.Repr.ToRepr, Data.Repr.FromRepr ) ! -> then add the same to patch

-- TODO: Path.connect2 (from different patches)

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
            sendToBIfFlagIsOn din = do
                -- Monad.whenM
                flagOn <- Ref.read flagRef
                if flagOn then sendInE nodeB inputB din
                else pure unit
        Signal.runSignal $ subscribeOutput (Record.get $ proxify outputA) nodeA ~> convert ~> sendToBIfFlagIsOn
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
            sendToBIfFlagIsOn din = do
                -- Monad.whenM
                flagOn <- Ref.read flagRef
                if flagOn then sendInE' nodeB inputB din
                else pure unit
        Signal.runSignal $ subscribeOutput (Record.get $ proxify outputA) nodeA ~> convert ~> sendToBIfFlagIsOn
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


connectByRepr
    :: forall fA fB oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' m repr
     . MonadEffect m
    => MonadRec m
    => HasOutput oA doutA osA' osA
    => HasInput iB dinB isB' isB
    => ToRepr doutA repr
    => FromRepr repr dinB
    => Proxy repr
    -> Output oA
    -> Input iB
    -> Node fA stateA isA osA m
    -> Node fB stateB isB osB m
    -> m (Link fA fB oA iB)
connectByRepr
    _
    outputA
    inputB
    nodeA@(Node nodeAId _ _ _)
    nodeB@(Node nodeBId _ _ _) =
    liftEffect $ do
        flagRef <- Ref.new true
        let
            convert :: doutA -> Maybe dinB
            convert dout = (toRepr dout :: Maybe (Repr repr)) >>= fromRepr
            sendToBWhenConditionsMet :: Maybe dinB -> Effect Unit
            sendToBWhenConditionsMet (Just din) = do
                -- Monad.whenM
                flagOn <- Ref.read flagRef
                if flagOn then sendInE nodeB inputB din
                else pure unit
            sendToBWhenConditionsMet Nothing =
                pure unit
        Signal.runSignal $ subscribeOutput (Record.get $ proxify outputA) nodeA ~> convert ~> sendToBWhenConditionsMet
        pure $ Link nodeAId (output' outputA) (input' inputB) nodeBId $ Ref.write false flagRef


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


inputsShapeHeld :: forall f state (is :: Row Type) os m rli. KH.KeysO rli Input HoldsInput => HasInputsAt is rli => Node f state is os m -> Array HoldsInput
inputsShapeHeld (Node _ _ _ fn) = Fn.inputsShapeHeld fn


outputsShapeHeld :: forall f state is (os :: Row Type) m rlo. KH.KeysO rlo Output HoldsOutput => HasOutputsAt os rlo => Node f state is os m -> Array HoldsOutput
outputsShapeHeld (Node _ _ _ fn) = Fn.outputsShapeHeld fn

{-
inputsShapeInNode :: forall f state (is :: Row Type) os m rli. KH.KeysO rli Input HoldsInputInNode => HasInputsAt is rli => Node f state is os m -> Array HoldsInputInNode
inputsShapeInNode (Node _ _ _ fn) = KH.orderedKeys' (Proxy :: _ Input) (Fn.inputsOrder fn) (Proxy :: _ is)


outputsShapeInNode :: forall f state is (os :: Row Type) m rlo. KH.KeysO rlo Output HoldsOutputInNode => HasOutputsAt os rlo => Node f state is os m -> Array HoldsOutputInNode
outputsShapeInNode (Node _ _ _ fn) = KH.orderedKeys' (Proxy :: _ Output) (Fn.outputsOrder fn) (Proxy :: _ os)
-}


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


shapeHeld
    :: forall f state (is :: Row Type) (os :: Row Type) m rli rlo
     . KH.KeysO rli Input HoldsInput
    => KH.KeysO rlo Output HoldsOutput
    => HasInputsAt is rli
    => HasOutputsAt os rlo
    => Node f state is os m
    -> Array HoldsInput /\ Array HoldsOutput
shapeHeld node = inputsShapeHeld node /\ outputsShapeHeld node


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


withNode2
  :: forall r
   . HoldsNode
  -> HoldsNode
  -> (forall fA stateA isA osA fB stateB isB osB mA mB. IsSymbol fA => IsSymbol fB => Node fA stateA isA osA mA -> Node fB stateB isB osB mB -> r)
  -> r
withNode2 holdsFA holdsFB fn =
  withNode
      holdsFA
      (\nodeA ->
            withNode
              holdsFB
              (\nodeB ->
                  fn nodeA nodeB
              )
      )

withNode2'
  :: forall fA fB m r
   . HoldsNode' fA m
  -> HoldsNode' fB m
  -> (forall stateA isA osA stateB isB osB. IsSymbol fA => IsSymbol fB => Node fA stateA isA osA m -> Node fB stateB isB osB m -> r)
  -> r
withNode2' holdsFA holdsFB fn =
  withNode'
      holdsFA
      (\nodeA ->
            withNode'
              holdsFB
              (\nodeB ->
                  fn nodeA nodeB
              )
      )


findInputByIndex :: forall f state i din is is' os m. IsSymbol i => HasInput i din is' is => Int -> Node f state is os m -> Maybe (Input i /\ Proxy din)
findInputByIndex _ _ = Nothing


findInput :: forall f state i din is is' os m. IsSymbol i => HasInput i din is' is => InputR -> Node f state is os m -> Maybe (Input i /\ Proxy din)
findInput _ _ = Nothing


findHeldInput :: forall f state i din is is' os m repr. IsSymbol f => HasInput i din is' is => ReadWriteRepr repr => ToRepr din repr => FromRepr repr din => Node f state is os m -> InputR -> Maybe (HoldsInputInNodeMRepr m repr)
findHeldInput node n = case findInput n node of
    Just ((input :: Input i) /\ (pdin :: Proxy din)) -> Just $ holdInputInNodeMRepr pdin node input
    Nothing -> Nothing


findHeldInputByIndex :: forall f state i din is is' os m repr. IsSymbol f => HasInput i din is' is => ReadWriteRepr repr => ToRepr din repr => FromRepr repr din => Node f state is os m -> Int -> Maybe (HoldsInputInNodeMRepr m repr)
findHeldInputByIndex node n = case findInputByIndex n node of
    Just ((input :: Input i) /\ (pdin :: Proxy din)) -> Just $ holdInputInNodeMRepr pdin node input
    Nothing -> Nothing


findOutputByIndex :: forall f state o dout is os os' m. IsSymbol o => HasOutput o dout os' os => Int -> Node f state is os m -> Maybe (Output o /\ Proxy dout)
findOutputByIndex _ _ = Nothing


findOutput :: forall f state o dout is os os' m. IsSymbol o => HasOutput o dout os' os => InputR -> Node f state is os m -> Maybe (Output o /\ Proxy dout)
findOutput _ _ = Nothing


findHeldOutput :: forall f state o dout is os os' m repr. IsSymbol f => HasOutput o dout os' os => ReadWriteRepr repr => ToRepr dout repr => FromRepr repr dout => Node f state is os m -> InputR -> Maybe (HoldsOutputInNodeMRepr m repr)
findHeldOutput node n = case findOutput n node of
    Just ((output :: Output o) /\ (pdout :: Proxy dout)) -> Just $ holdOutputInNodeMRepr pdout node output
    Nothing -> Nothing


findHeldOutputByIndex :: forall f state o dout is os os' m repr. IsSymbol f => HasOutput o dout os' os => ReadWriteRepr repr => ToRepr dout repr => FromRepr repr dout => Node f state is os m -> Int -> Maybe (HoldsOutputInNodeMRepr m repr)
findHeldOutputByIndex node n = case findOutputByIndex n node of
    Just ((output :: Output o) /\ (pdout :: Proxy dout)) -> Just $ holdOutputInNodeMRepr pdout node output
    Nothing -> Nothing



newtype HoldsInputInNode = HoldsInputInNode (forall r. (forall f state i din is is' os m. IsSymbol f => HasInput i din is' is => Node f state is os m -> Input i -> r) -> r)
newtype HoldsInputInNode' f m = HoldsInputInNode' (forall r. (forall state i din is is' os. IsSymbol f => HasInput i din is' is => Node f state is os m -> Input i -> r) -> r)
newtype HoldsInputInNode'' f state is os m = HoldsInputInNode'' (forall r. (forall i din is'. IsSymbol f => HasInput i din is' is => Node f state is os m -> Input i -> r) -> r)
newtype HoldsInputInNodeM m = HoldsInputInNodeM (forall r. (forall f state i din is is' os. IsSymbol f => HasInput i din is' is => Node f state is os m -> Input i -> r) -> r)
newtype HoldsInputInNodeMRepr m repr = HoldsInputInNodeMRepr (forall r. (forall f state i din is is' os. IsSymbol f => HasInput i din is' is => ReadWriteRepr repr => ToRepr din repr => FromRepr repr din => Proxy din -> Node f state is os m -> Input i -> r) -> r)


holdInputInNode :: forall f state i din is is' os m. IsSymbol f => HasInput i din is' is => Node f state is os m -> Input i -> HoldsInputInNode
holdInputInNode node input = HoldsInputInNode \f -> f node input


holdInputInNode' :: forall f state i din is is' os m. IsSymbol f => HasInput i din is' is => Node f state is os m -> Input i -> HoldsInputInNode' f m
holdInputInNode' node input = HoldsInputInNode' \f -> f node input


holdInputInNode'' :: forall f state i din is is' os m. IsSymbol f => HasInput i din is' is => Node f state is os m -> Input i -> HoldsInputInNode'' f state is os m
holdInputInNode'' node input = HoldsInputInNode'' \f -> f node input


holdInputInNodeM :: forall f state i din is is' os m. IsSymbol f => HasInput i din is' is => Node f state is os m -> Input i -> HoldsInputInNodeM m
holdInputInNodeM node input = HoldsInputInNodeM \f -> f node input


holdInputInNodeMRepr :: forall f state i din is is' os m repr. IsSymbol f => IsSymbol i => HasInput i din is' is => ReadWriteRepr repr => ToRepr din repr => FromRepr repr din => Proxy din -> Node f state is os m -> Input i -> HoldsInputInNodeMRepr m repr
holdInputInNodeMRepr p node input = HoldsInputInNodeMRepr \f -> f p node input


withInputInNode :: forall r. HoldsInputInNode -> (forall f state i din is is' os m. IsSymbol f => HasInput i din is' is => Node f state is os m -> Input i -> r) -> r
withInputInNode (HoldsInputInNode f) = f


withInputInNode' :: forall f m r. HoldsInputInNode' f m -> (forall state i din is is' os. IsSymbol f => HasInput i din is' is => Node f state is os m -> Input i -> r)  -> r
withInputInNode' (HoldsInputInNode' f) = f


withInputInNode'' :: forall f state is os m r. HoldsInputInNode'' f state is os m -> (forall i din is'. IsSymbol f => HasInput i din is' is => Node f state is os m -> Input i -> r) -> r
withInputInNode'' (HoldsInputInNode'' f) = f


withInputInNodeM :: forall r m. HoldsInputInNodeM m -> (forall f state i din is is' os. IsSymbol f => HasInput i din is' is => Node f state is os m -> Input i -> r) -> r
withInputInNodeM (HoldsInputInNodeM f) = f


withInputInNodeMRepr :: forall r m repr. HoldsInputInNodeMRepr m repr -> (forall f state i din is is' os. IsSymbol f => HasInput i din is' is => ReadWriteRepr repr => ToRepr din repr => FromRepr repr din => Proxy din -> Node f state is os m -> Input i -> r) -> r
withInputInNodeMRepr (HoldsInputInNodeMRepr f) = f


newtype HoldsOutputInNode = HoldsOutputInNode (forall r. (forall f state o dout is os os' m. IsSymbol f => HasOutput o dout os' os => Node f state is os m -> Output o -> r) -> r)
newtype HoldsOutputInNode' f m = HoldsOutputInNode' (forall r. (forall state o dout is os os'. IsSymbol f => HasOutput o dout os' os => Node f state is os m -> Output o -> r) -> r)
newtype HoldsOutputInNode'' f state is os m = HoldsOutputInNode'' (forall r. (forall o dout os'. IsSymbol f => HasOutput o dout os' os => Node f state is os m -> Output o -> r) -> r)
newtype HoldsOutputInNodeM m = HoldsOutputInNodeM (forall r. (forall f state o dout is os os'. IsSymbol f => HasOutput o dout os' os => Node f state is os m -> Output o -> r) -> r)
newtype HoldsOutputInNodeMRepr m repr = HoldsOutputInNodeMRepr (forall r. (forall f state o dout is os os'. IsSymbol f => HasOutput o dout os' os => ReadWriteRepr repr => ToRepr dout repr => FromRepr repr dout => Proxy dout -> Node f state is os m -> Output o -> r) -> r)


instance Show (HoldsInputInNodeMRepr m repr) where
    show hinput = withInputInNodeMRepr hinput \_ _ input -> reflect input


instance Show (HoldsOutputInNodeMRepr m repr) where
    show houtput = withOutputInNodeMRepr houtput \_ _ output -> reflect output


holdOutputInNode :: forall f state o dout is os os' m. IsSymbol f => HasOutput o dout os' os => Node f state is os m -> Output o -> HoldsOutputInNode
holdOutputInNode node output = HoldsOutputInNode \f -> f node output


holdOutputInNode' :: forall f state o dout is os os' m. IsSymbol f => HasOutput o dout os' os => Node f state is os m -> Output o -> HoldsOutputInNode' f m
holdOutputInNode' node output = HoldsOutputInNode' \f -> f node output


holdOutputInNode'' :: forall f state o dout is os os' m. IsSymbol f => HasOutput o dout os' os => Node f state is os m -> Output o -> HoldsOutputInNode'' f state is os m
holdOutputInNode'' node output = HoldsOutputInNode'' \f -> f node output


holdOutputInNodeM :: forall f state o dout is os os' m. IsSymbol f => HasOutput o dout os' os => Node f state is os m -> Output o -> HoldsOutputInNodeM m
holdOutputInNodeM node output = HoldsOutputInNodeM \f -> f node output


holdOutputInNodeMRepr :: forall f state o dout is os os' m repr. IsSymbol f => HasOutput o dout os' os => ReadWriteRepr repr => ToRepr dout repr => FromRepr repr dout => Proxy dout -> Node f state is os m -> Output o -> HoldsOutputInNodeMRepr m repr
holdOutputInNodeMRepr p node output = HoldsOutputInNodeMRepr \f -> f p node output


withOutputInNode :: forall r. HoldsOutputInNode -> (forall f state o dout is os os' m. IsSymbol f => HasOutput o dout os' os => Node f state is os m -> Output o -> r) -> r
withOutputInNode (HoldsOutputInNode f) = f


withOutputInNode' :: forall f m r. HoldsOutputInNode' f m -> (forall state o dout is os os'. IsSymbol f => HasOutput o dout os' os => Node f state is os m -> Output o -> r)  -> r
withOutputInNode' (HoldsOutputInNode' f) = f


withOutputInNode'' :: forall f state is os m r. HoldsOutputInNode'' f state is os m -> (forall o dout os'. IsSymbol f => HasOutput o dout os' os => Node f state is os m -> Output o -> r) -> r
withOutputInNode'' (HoldsOutputInNode'' f) = f


withOutputInNodeM :: forall m r. HoldsOutputInNodeM m -> (forall f state o dout is os os'. IsSymbol f => HasOutput o dout os' os => Node f state is os m -> Output o -> r)  -> r
withOutputInNodeM (HoldsOutputInNodeM f) = f


withOutputInNodeMRepr :: forall m repr r. HoldsOutputInNodeMRepr m repr -> (forall f state o dout is os os'. IsSymbol f => HasOutput o dout os' os => ReadWriteRepr repr => ToRepr dout repr => FromRepr repr dout => Proxy dout -> Node f state is os m -> Output o -> r) -> r
withOutputInNodeMRepr (HoldsOutputInNodeMRepr f) = f


{-
instance KH.Holder1 Input (Node f state is os m) HoldsInputInNode where
    hold1 :: forall i din is'. IsSymbol i => HasInput i din is' is => Node f state is os m -> Input i -> HoldsInputInNode
    hold1 = holdInputInNode
    extract1 :: forall r is'. HoldsInputInNode -> (forall i din is'. IsSymbol i => HasInput i din is' is => Node f state is os m -> Input i -> r) -> r
    extract1 = withInputInNode
-}


{-
class KeysO (xs :: RL.RowList Type) (proxy :: Symbol -> Type) x where
  keysImplO :: Proxy proxy -> SOrder -> Proxy xs -> Array (Int /\ x)

instance nilKeysO :: KeysO RL.Nil proxy x where
  keysImplO _ _ _ = mempty
else instance consKeysO ::
  ( IsSymbol name
  , Holder proxy x
  , ReifyOrderedTo proxy
  , KeysO tail proxy x
  ) => KeysO (RL.Cons name ty tail) proxy x where
  keysImplO :: forall xs. Proxy proxy -> SOrder -> Proxy xs -> Array (Int /\ x)
  keysImplO p order _ =
    Array.insertBy cmpF (index /\ held) ordered
    where
      cmpF tupleA tupleB = compare (Tuple.fst tupleA) (Tuple.fst tupleB)
      index = SOrder.indexOf order (Proxy :: _ name)
      held = hold (reifyAt index (Proxy :: Proxy name) :: proxy name)
      ordered = keysImplO p order (Proxy :: _ tail)
-}


class (IsSymbol f) <= HoldsInputs (is :: Row Type) (rli :: RL.RowList Type) f state os m | is -> rli where
    holdInputs :: Proxy rli -> Node f state is os m -> Array (Int /\ HoldsInputInNode)


instance nilHoldsInputs :: (IsSymbol f) => HoldsInputs is RL.Nil f state os m where
  holdInputs :: Proxy RL.Nil -> Node f state is os m -> Array (Int /\ HoldsInputInNode)
  holdInputs _ _ = mempty
else instance consHoldsInputs ::
  ( IsSymbol f, HasInput i din is' is
  , HasInputsAt is tail
  , HoldsInputs is tail f state os m
  ) => HoldsInputs is (RL.Cons i din tail) f state os m where
  holdInputs :: forall rli. Proxy rli -> Node f state is os m -> Array (Int /\ HoldsInputInNode)
  holdInputs _ node =
    Array.insertBy cmpF (index /\ holdInputInNode node (Input index :: _ i)) (holdInputs (Proxy :: _ tail) node)
    where
      order = inputsOrder node
      cmpF tupleA tupleB = compare (Tuple.fst tupleA) (Tuple.fst tupleB)
      index = SOrder.indexOf order (Proxy :: _ i)
    --   held = hold1 s (reifyAt index (Proxy :: Proxy name) :: proxy name)
    --   ordered = inputsHeld (Proxy :: _ tail)


orderedInputs :: forall rli f state is os m
   . IsSymbol f
  => HoldsInputs is rli f state os m
  => Node f state is os m
  -> Array HoldsInputInNode
orderedInputs node = Tuple.snd <$> holdInputs (Proxy :: _ rli) node


instance Reflect' HoldsInputInNode where
    reflect' hiin = withInputInNode hiin (const reflect)


class (IsSymbol f) <= HoldsOutputs (os :: Row Type) (rlo :: RL.RowList Type) f state is m | os -> rlo where
    holdOutputs :: Proxy rlo -> Node f state is os m -> Array (Int /\ HoldsOutputInNode)


instance nilHoldsOutputs :: (IsSymbol f) => HoldsOutputs os RL.Nil f state is m where
  holdOutputs :: Proxy RL.Nil -> Node f state is os m -> Array (Int /\ HoldsOutputInNode)
  holdOutputs _ _ = mempty
else instance consHoldsOutputs ::
  ( IsSymbol f, HasOutput o dout os' os
--   , HasInputsAt is (RL.Cons i din tail)
  , HasOutputsAt os tail
  , HoldsOutputs os tail f state is m
  ) => HoldsOutputs os (RL.Cons o dout tail) f state is m where
  holdOutputs :: forall rlo. Proxy rlo -> Node f state is os m -> Array (Int /\ HoldsOutputInNode)
  holdOutputs _ node =
    Array.insertBy cmpF (index /\ holdOutputInNode node (Output index :: _ o)) (holdOutputs (Proxy :: _ tail) node)
    where
      order = outputsOrder node
      cmpF tupleA tupleB = compare (Tuple.fst tupleA) (Tuple.fst tupleB)
      index = SOrder.indexOf order (Proxy :: _ o)
    --   held = hold1 s (reifyAt index (Proxy :: Proxy name) :: proxy name)
    --   ordered = inputsHeld (Proxy :: _ tail)


orderedOutputs :: forall rlo f state is os m
   . IsSymbol f
  => HoldsOutputs os rlo f state is m
  => Node f state is os m
  -> Array HoldsOutputInNode
orderedOutputs node = Tuple.snd <$> holdOutputs (Proxy :: _ rlo) node


instance Reflect' HoldsOutputInNode where
    reflect' hoin = withOutputInNode hoin (const reflect)



class (IsSymbol f) <= HoldsInputsM (is :: Row Type) (rli :: RL.RowList Type) f state os m | is -> rli where
    holdInputsM :: Proxy rli -> Node f state is os m -> Array (Int /\ HoldsInputInNodeM m)


instance nilHoldsInputsM :: (IsSymbol f) => HoldsInputsM is RL.Nil f state os m where
  holdInputsM :: Proxy RL.Nil -> Node f state is os m -> Array (Int /\ HoldsInputInNodeM m)
  holdInputsM _ _ = mempty
else instance consHoldsInputsM ::
  ( IsSymbol f, HasInput i din is' is
--   , HasInputsAt is (RL.Cons i din tail)
  , HasInputsAt is tail
  , HoldsInputsM is tail f state os m
  ) => HoldsInputsM is (RL.Cons i din tail) f state os m where
  holdInputsM :: forall rli. Proxy rli -> Node f state is os m -> Array (Int /\ HoldsInputInNodeM m)
  holdInputsM _ node =
    Array.insertBy cmpF (index /\ holdInputInNodeM node (Input index :: _ i)) (holdInputsM (Proxy :: _ tail) node)
    where
      order = inputsOrder node
      cmpF tupleA tupleB = compare (Tuple.fst tupleA) (Tuple.fst tupleB)
      index = SOrder.indexOf order (Proxy :: _ i)
    --   held = hold1 s (reifyAt index (Proxy :: Proxy name) :: proxy name)
    --   ordered = inputsHeld (Proxy :: _ tail)


orderedInputsM :: forall rli f state is os m
   . IsSymbol f
  => HoldsInputsM is rli f state os m
  => Node f state is os m
  -> Array (HoldsInputInNodeM m)
orderedInputsM node =
  Tuple.snd <$> holdInputsM (Proxy :: _ rli) node


instance Reflect' (HoldsInputInNodeM m) where
    reflect' hiin = withInputInNodeM hiin (const reflect)


class (IsSymbol f) <= HoldsOutputsM (os :: Row Type) (rlo :: RL.RowList Type) f state is m | os -> rlo where
    holdOutputsM :: Proxy rlo -> Node f state is os m -> Array (Int /\ HoldsOutputInNodeM m)


instance nilHoldsOutputsM :: (IsSymbol f) => HoldsOutputsM os RL.Nil f state is m where
  holdOutputsM :: Proxy RL.Nil -> Node f state is os m -> Array (Int /\ HoldsOutputInNodeM m)
  holdOutputsM _ _ = mempty
else instance consHoldsOutputsM ::
  ( IsSymbol f, HasOutput o dout os' os
--   , HasInputsAt is (RL.Cons i din tail)
  , HasOutputsAt os tail
  , HoldsOutputsM os tail f state is m
  ) => HoldsOutputsM os (RL.Cons o dout tail) f state is m where
  holdOutputsM :: forall rlo. Proxy rlo -> Node f state is os m -> Array (Int /\ HoldsOutputInNodeM m)
  holdOutputsM _ node =
    Array.insertBy cmpF (index /\ holdOutputInNodeM node (Output index :: _ o)) (holdOutputsM (Proxy :: _ tail) node)
    where
      order = outputsOrder node
      cmpF tupleA tupleB = compare (Tuple.fst tupleA) (Tuple.fst tupleB)
      index = SOrder.indexOf order (Proxy :: _ o)
    --   held = hold1 s (reifyAt index (Proxy :: Proxy name) :: proxy name)
    --   ordered = inputsHeld (Proxy :: _ tail)


orderedOutputsM :: forall rlo f state is os m
   . IsSymbol f
  => HoldsOutputsM os rlo f state is m
  => Node f state is os m
  -> Array (HoldsOutputInNodeM m)
orderedOutputsM node =
  Tuple.snd <$> holdOutputsM (Proxy :: _ rlo) node


instance Reflect' (HoldsOutputInNodeM m) where
    reflect' hoin = withOutputInNodeM hoin (const reflect)



class (IsSymbol f, ReadWriteRepr repr, FromToReprRow rli is repr) <= HoldsInputsMRepr (is :: Row Type) (rli :: RL.RowList Type) f state os m repr | is -> rli where
    holdInputsMRepr :: Proxy rli -> Node f state is os m -> Array (Int /\ HoldsInputInNodeMRepr m repr)


instance nilHoldsInputsMRepr :: (IsSymbol f, ReadWriteRepr repr) => HoldsInputsMRepr is RL.Nil f state os m repr where
  holdInputsMRepr :: Proxy RL.Nil -> Node f state is os m -> Array (Int /\ HoldsInputInNodeMRepr m repr)
  holdInputsMRepr _ _ = mempty
else instance consHoldsInputsMRepr ::
  ( IsSymbol f
  , HasInput i din is' is
--   , HasInputsAt is (RL.Cons i din tail)
  , HasInputsAt is tail
  , ToRepr din repr
  , FromRepr repr din
  , HoldsInputsMRepr is tail f state os m repr
  ) => HoldsInputsMRepr is (RL.Cons i din tail) f state os m repr where
  holdInputsMRepr :: forall rli. Proxy rli -> Node f state is os m -> Array (Int /\ HoldsInputInNodeMRepr m repr)
  holdInputsMRepr _ node =
    Array.insertBy cmpF (index /\ holdInputInNodeMRepr (Proxy :: _ din) node (Input index :: _ i)) (holdInputsMRepr (Proxy :: _ tail) node)
    where
      order = inputsOrder node
      cmpF tupleA tupleB = compare (Tuple.fst tupleA) (Tuple.fst tupleB)
      index = SOrder.indexOf order (Proxy :: _ i)
    --   held = hold1 s (reifyAt index (Proxy :: Proxy name) :: proxy name)
    --   ordered = inputsHeld (Proxy :: _ tail)


orderedInputsMRepr :: forall rli f state is os m repr
   . IsSymbol f
  => HoldsInputsMRepr is rli f state os m repr
  => Node f state is os m
  -> Array (HoldsInputInNodeMRepr m repr)
orderedInputsMRepr node = Tuple.snd <$> holdInputsMRepr (Proxy :: _ rli) node


instance Reflect' (HoldsInputInNodeMRepr m repr) where
    reflect' hiinr = withInputInNodeMRepr hiinr (const $ const reflect)


class (IsSymbol f, ReadWriteRepr repr, FromToReprRow rlo os repr) <= HoldsOutputsMRepr (os :: Row Type) (rlo :: RL.RowList Type) f state is m repr | os -> rlo where
    holdOutputsMRepr :: Proxy rlo -> Node f state is os m -> Array (Int /\ HoldsOutputInNodeMRepr m repr)


instance nilHoldsOutputsMRepr :: (IsSymbol f, ReadWriteRepr repr) => HoldsOutputsMRepr os RL.Nil f state is m repr where
  holdOutputsMRepr :: Proxy RL.Nil -> Node f state is os m -> Array (Int /\ HoldsOutputInNodeMRepr m repr)
  holdOutputsMRepr _ _ = mempty
else instance consHoldsOutputsMRepr ::
  ( IsSymbol f, HasOutput o dout os' os
--   , HasInputsAt is (RL.Cons i din tail)
  , HasOutputsAt os tail
  , ToRepr dout repr
  , FromRepr repr dout
  , HoldsOutputsMRepr os tail f state is m repr
  ) => HoldsOutputsMRepr os (RL.Cons o dout tail) f state is m repr where
  holdOutputsMRepr :: forall rlo. Proxy rlo -> Node f state is os m -> Array (Int /\ HoldsOutputInNodeMRepr m repr)
  holdOutputsMRepr _ node =
    Array.insertBy cmpF (index /\ holdOutputInNodeMRepr (Proxy :: _ dout) node (Output index :: _ o)) (holdOutputsMRepr (Proxy :: _ tail) node)
    where
      order = outputsOrder node
      cmpF tupleA tupleB = compare (Tuple.fst tupleA) (Tuple.fst tupleB)
      index = SOrder.indexOf order (Proxy :: _ o)
    --   held = hold1 s (reifyAt index (Proxy :: Proxy name) :: proxy name)
    --   ordered = inputsHeld (Proxy :: _ tail)


orderedOutputsMRepr :: forall rlo f state is os m repr
   . IsSymbol f
  => HoldsOutputsMRepr os rlo f state is m repr
  => Node f state is os m
  -> Array (HoldsOutputInNodeMRepr m repr)
orderedOutputsMRepr node = Tuple.snd <$> holdOutputsMRepr (Proxy :: _ rlo) node


instance Reflect' (HoldsOutputInNodeMRepr m repr) where
    reflect' hoinr = withOutputInNodeMRepr hoinr (const $ const reflect)



-- TODO: add Node here

class Holder (proxy :: Symbol -> Type) a x where
  hold :: forall sym. IsSymbol sym => proxy sym -> a -> x


instance Holder Input Unit HoldsInput where
  hold :: forall sym. IsSymbol sym => Input sym -> Unit -> HoldsInput
  hold input _ = KH.hold input


instance Holder Input (Node f state is os m) HoldsInput where
  hold :: forall sym. IsSymbol sym => Input sym -> Node f state is os m -> HoldsInput
  hold input _ = KH.hold input


class HolderI :: forall k1 k2. (Symbol -> Type) -> Symbol -> k1 -> Row Type -> k2 -> Symbol -> (Type -> Type) -> Type -> Constraint
class HolderI (proxy :: Symbol -> Type) i din is is' f m x | i -> din, is -> is', is -> m, is -> f where
  holdI :: forall state os. proxy i -> Node f state is os m -> x


class HolderO :: forall k1 k2. (Symbol -> Type) -> Symbol -> k1 -> Row Type -> k2 -> Symbol -> (Type -> Type) -> Type -> Constraint
class HolderO (proxy :: Symbol -> Type) o dout os os' f m x | o -> dout, os -> os', os -> m, os -> f where
  holdO :: forall state is. proxy o -> Node f state is os m -> x


instance (IsSymbol f, IsSymbol i, HasInput i din is' is) => HolderI Input i din is is' f m (HoldsInputInNodeM m) where
  holdI :: forall state os. Input i -> Node f state is os m -> HoldsInputInNodeM m
  holdI input node = holdInputInNodeM node input


instance (IsSymbol f, IsSymbol i, HasInput i din is' is, ReadWriteRepr repr, ToRepr din repr, FromRepr repr din) => HolderI Input i din is is' f m (HoldsInputInNodeMRepr m repr) where
  holdI :: forall state os. IsSymbol f => Input i -> Node f state is os m -> HoldsInputInNodeMRepr m repr
  holdI input node = holdInputInNodeMRepr (Proxy :: _ din) node input


instance (IsSymbol f, IsSymbol o, HasOutput o dout os' os) => HolderO Output o dout os os' f m (HoldsOutputInNodeM m) where
  holdO :: forall state is. Output o -> Node f state is os m -> HoldsOutputInNodeM m
  holdO output node = holdOutputInNodeM node output


instance (IsSymbol f, IsSymbol o, HasOutput o dout os' os, ReadWriteRepr repr, ToRepr dout repr, FromRepr repr dout) => HolderO Output o dout os os' f m (HoldsOutputInNodeMRepr m repr) where
  holdO :: forall state is. IsSymbol f => Output o -> Node f state is os m -> HoldsOutputInNodeMRepr m repr
  holdO output node = holdOutputInNodeMRepr (Proxy :: _ dout) node output



{- instance (IsSymbol f, HasInputsAt is irl) => Holder Input (Node f state is os m) (HoldsInputInNode'' f state is os m) where
  hold :: forall i din is'. IsSymbol i => HasInput i din is' is => Input i -> Node f state is os m -> HoldsInputInNode'' f state is os m
  hold input node = holdInputInNode'' node input -}



class ReifyOrderedTo proxy a where
  reifyAt :: forall sym. IsSymbol sym => Int -> Proxy sym -> a -> proxy sym


instance ReifyOrderedTo Input Unit where
  reifyAt :: forall sym. IsSymbol sym => Int -> Proxy sym -> Unit -> Input sym
  reifyAt n _ _ = Input n


instance ReifyOrderedTo Input (Node f state is os m) where
  reifyAt :: forall sym. IsSymbol sym => Int -> Proxy sym -> (Node f state is os m) -> Input sym
  reifyAt n _ _ = Input n


instance ReifyOrderedTo Output (Node f state is os m) where
  reifyAt :: forall sym. IsSymbol sym => Int -> Proxy sym -> (Node f state is os m) -> Output sym
  reifyAt n _ _ = Output n


class SubjectBoundKeys (xs :: RL.RowList Type) (proxy :: Symbol -> Type) a x where
  subjectBoundKeysImpl :: Proxy proxy -> SOrder -> Proxy xs -> a -> Array (Int /\ x)


instance nilKeysTest :: SubjectBoundKeys RL.Nil proxy a x where
  subjectBoundKeysImpl _ _ _ _ = mempty
else instance consKeysTest ::
  ( IsSymbol name
  , Holder proxy a x
  , ReifyOrderedTo proxy a
  , SubjectBoundKeys tail proxy a x
  ) => SubjectBoundKeys (RL.Cons name ty tail) proxy a x where
  subjectBoundKeysImpl :: forall xs. Proxy proxy -> SOrder -> Proxy xs -> a -> Array (Int /\ x)
  subjectBoundKeysImpl p order _ a =
    Array.insertBy cmpF (index /\ held) ordered
    where
      cmpF tupleA tupleB = compare (Tuple.fst tupleA) (Tuple.fst tupleB)
      index = SOrder.indexOf order (Proxy :: _ name)
      held = hold (reifyAt index (Proxy :: Proxy name) a :: proxy name) a
      ordered = subjectBoundKeysImpl p order (Proxy :: _ tail) a


class NodeBoundKeys k (xs :: RL.RowList Type) (proxy :: Symbol -> Type) f state is os m x where
  nodeBoundKeysImpl :: Proxy k -> Proxy proxy -> SOrder -> Proxy xs -> Node f state is os m -> Array (Int /\ x)


{-
Noodle.Node2.NodeBoundKeys I
                                rli7
                                Input
                                f8
                                state9
                                is10
                                os11
                                Effect
                                (HoldsInputInNodeMRepr Effect WrapRepr) -}





instance nilNKeysITest :: NodeBoundKeys k RL.Nil proxy f state is os m x where
  nodeBoundKeysImpl _ _ _ _ = mempty
else instance consNKeysITest ::
  ( IsSymbol name
  , HolderI proxy name ty is is' f m x
  , ReifyOrderedTo proxy (Node f state is os m)
  , NodeBoundKeys I tail proxy f state is os m x
  ) => NodeBoundKeys I (RL.Cons name ty tail) proxy f state is os m x where
  nodeBoundKeysImpl :: forall xs. Proxy I -> Proxy proxy -> SOrder -> Proxy xs -> Node f state is os m -> Array (Int /\ x)
  nodeBoundKeysImpl pi p order _ node =
    Array.insertBy cmpF (index /\ held) ordered
    where
      cmpF tupleA tupleB = compare (Tuple.fst tupleA) (Tuple.fst tupleB)
      index = SOrder.indexOf order (Proxy :: _ name)
      held = holdI (reifyAt index (Proxy :: Proxy name) node :: proxy name) node
      ordered = nodeBoundKeysImpl pi p order (Proxy :: _ tail) node
else instance consNKeysOTest ::
  ( IsSymbol name
  , HolderO proxy name ty os os' f m x
  , ReifyOrderedTo proxy (Node f state is os m)
  , NodeBoundKeys O tail proxy f state is os m x
  ) => NodeBoundKeys O (RL.Cons name ty tail) proxy f state is os m x where
  nodeBoundKeysImpl :: forall xs. Proxy O -> Proxy proxy -> SOrder -> Proxy xs -> Node f state is os m -> Array (Int /\ x)
  nodeBoundKeysImpl po p order _ node =
    Array.insertBy cmpF (index /\ held) ordered
    where
      cmpF tupleA tupleB = compare (Tuple.fst tupleA) (Tuple.fst tupleB)
      index = SOrder.indexOf order (Proxy :: _ name)
      held = holdO (reifyAt index (Proxy :: Proxy name) node :: proxy name) node
      ordered = nodeBoundKeysImpl po p order (Proxy :: _ tail) node
else instance consNKeysITest' ::
  ( IsSymbol name
  , HolderI Input name ty is is' f m (HoldsInputInNodeMRepr m repr)
  , ReifyOrderedTo Input (Node f state is os m)
  , NodeBoundKeys I tail Input f state is os m (HoldsInputInNodeMRepr m repr)
  ) => NodeBoundKeys I (RL.Cons name ty tail) Input f state is os m (HoldsInputInNodeMRepr m repr) where
  nodeBoundKeysImpl :: forall xs. Proxy I -> Proxy Input -> SOrder -> Proxy xs -> Node f state is os m -> Array (Int /\ (HoldsInputInNodeMRepr m repr))
  nodeBoundKeysImpl pi p order _ node =
    Array.insertBy cmpF (index /\ held) ordered
    where
      cmpF tupleA tupleB = compare (Tuple.fst tupleA) (Tuple.fst tupleB)
      index = SOrder.indexOf order (Proxy :: _ name)
      held = holdI (reifyAt index (Proxy :: Proxy name) node :: Input name) node
      ordered = nodeBoundKeysImpl pi p order (Proxy :: _ tail) node


orderedNodeBoundKeysTest :: forall g row rl proxy x
   . RL.RowToList row rl
  => SubjectBoundKeys rl proxy Unit x
  => Proxy proxy
  -> SOrder
  -> g row -- this will work for any type with the row as a param!
  -> Array x
orderedNodeBoundKeysTest p order _ = Tuple.snd <$> subjectBoundKeysImpl p order (Proxy :: _ rl) unit


orderedNodeBoundKeysTest' :: forall g row rl proxy x f state is os m
   . RL.RowToList row rl
  => SubjectBoundKeys rl proxy (Node f state is os m) x
  => Proxy proxy
  -> SOrder
  -> g row -- this will work for any type with the row as a param!
  -> Node f state is os m
  -> Array x
orderedNodeBoundKeysTest' p order _ node = Tuple.snd <$> subjectBoundKeysImpl p order (Proxy :: _ rl) node


orderedNodeInputsTest :: forall rl iholder f state is os m
   . HasInputsAt is rl
  => SubjectBoundKeys rl Input (Node f state is os m) iholder
  => Node f state is os m
  -> Array iholder
orderedNodeInputsTest node = Tuple.snd <$> subjectBoundKeysImpl (Proxy :: _ Input) (inputsOrder node) (Proxy :: _ rl) node


data I

data O



orderedNodeInputsTest' :: forall rl iholder f state is os m
   . HasInputsAt is rl
  => NodeBoundKeys I rl Input f state is os m iholder
  => Node f state is os m
  -> Array iholder
orderedNodeInputsTest' node = Tuple.snd <$> nodeBoundKeysImpl (Proxy :: _ I) (Proxy :: _ Input) (inputsOrder node) (Proxy :: _ rl) node


orderedNodeInputsTest'' :: forall rl iholder f state is os m
   . HasInputsAt is rl
  => NodeBoundKeys I rl Input f state is os m iholder
  => Proxy rl
  -> Node f state is os m
  -> Array iholder
orderedNodeInputsTest'' prl node = Tuple.snd <$> nodeBoundKeysImpl (Proxy :: _ I) (Proxy :: _ Input) (inputsOrder node) prl node


orderedNodeOutputsTest' :: forall rl oholder f state is os m
   . HasOutputsAt os rl
  => NodeBoundKeys O rl Output f state is os m oholder
  => Node f state is os m
  -> Array oholder
orderedNodeOutputsTest' node = Tuple.snd <$> nodeBoundKeysImpl (Proxy :: _ O) (Proxy :: _ Output) (outputsOrder node) (Proxy :: _ rl) node