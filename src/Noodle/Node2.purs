module Noodle.Node2
  where


import Prelude

import Prim.RowList as RL
import Prim.Row as R
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol)

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
import Noodle.Fn2.Protocol (Protocol, Tracker, InputChange(..))
import Noodle.Fn2.Protocol as Protocol
import Noodle.Fn2.Flow (keysToInputs, keysToOutputs, InputId, OutputId, Input, Output, inputIdToString, outputIdToString, inputId) as Fn
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

type Family = String


type UID = Int


data Node state (is :: Row Type) (os :: Row Type) m = Node (Family /\ UID) (Tracker state is os) (Protocol state is os) (Fn state is os m)


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


make :: forall state is os m. MonadEffect m => (Family /\ UID) -> state -> Record is -> Record os -> ProcessM state is os m Unit -> m (Node state is os m)
make (family /\ uid) state is os process =
    make' (family /\ uid) state is os $ Fn.make family process


make' :: forall state is os m. MonadEffect m => (Family /\ UID) -> state -> Record is -> Record os -> Fn state is os m -> m (Node state is os m)
make' id state is os fn =
    Protocol.onChannels state is os
    <#> \(tracker /\ protocol) -> Node id tracker protocol fn


_in :: Fn.InputId -> String
_in = Fn._in


_out :: Fn.OutputId -> String
_out = Fn._out


{-}
mapM :: forall state is os m m'. (m ~> m') -> Fn state is os m -> Fn state is os m'
mapM f (Node id protocol processM) = Fn name state is os $ Process.mapMM f processM


imapState :: forall state state' is os m. (state -> state') -> (state' -> state) -> Fn state is os m -> Fn state' is os m
imapState f g (Fn name state is os processM) = Fn name (f state) is os $ Process.imapMState f g processM
-}

{- Running -}


run :: forall state is os m. MonadRec m => MonadEffect m => Node state is os m -> m Unit
run (Node _ _ protocol fn) =
    Fn.run' protocol fn


{- Get information about the function -}


family :: forall state is os m. Node state is os m -> Family
family (Node (family /\ _) _ _ _) = family


uid :: forall state is os m. Node state is os m -> UID
uid (Node (_ /\ uid) _ _ _) = uid


state :: forall state is os m. MonadEffect m => Node state is os m -> m state
state (Node _ _ protocol _) = liftEffect $ protocol.getState unit


inputs :: forall state is os m. MonadEffect m => Node state is os m -> m (Record is)
inputs (Node _ _ protocol _) = liftEffect $ Tuple.snd <$> protocol.getInputs unit


outputs :: forall state is os m. MonadEffect m => Node state is os m -> m (Record os)
outputs (Node _ _ protocol _) = liftEffect $ Tuple.snd <$> protocol.getOutputs unit


atInput :: forall i state is' is os m din. MonadEffect m => IsSymbol i => R.Cons i din is' is => Fn.Input i -> Node state is os m -> m din
atInput i node = inputs node <#> Record.get i


atOutput :: forall o state is os os' m dout. MonadEffect m => IsSymbol o => R.Cons o dout os' os => Fn.Output o -> Node state is os m -> m dout
atOutput o node = outputs node <#> Record.get o


atI :: forall i state is' is os m din. MonadEffect m => IsSymbol i => R.Cons i din is' is => Node state is os m -> Fn.Input i -> m din
atI = flip atInput


atO :: forall o state is os os' m dout. MonadEffect m => IsSymbol o => R.Cons o dout os' os => Node state is os m -> Fn.Output o -> m dout
atO = flip atOutput


-- at' ∷ ∀ (m ∷ Type -> Type) (t364 ∷ Type) (state ∷ Type) (os ∷ Row Type) (is ∷ Row Type) (dout :: Type). Functor m ⇒ Node state is os m → (Record is -> dout) -> m dout
_at ∷ forall m state is os din. MonadEffect m ⇒ Node state is os m → (Record is -> din) -> m din
_at node fn = inputs node <#> fn


at_ ∷ forall m state is os dout. MonadEffect m ⇒ Node state is os m → (Record os -> dout) -> m dout
at_ node fn = outputs node <#> fn


get :: forall state is os m. MonadEffect m => Node state is os m -> m ( state /\ Record is /\ Record os )
get node = do
    state <- state node
    is <- inputs node
    os <- outputs node
    pure $ state /\ is /\ os


subscribeInput :: forall state is os m din. (Record is -> din) -> Node state is os m -> Signal din
subscribeInput fn node = fn <$> subscribeInputs node


subscribeInputs :: forall state is os m. Node state is os m -> Signal (Record is)
subscribeInputs (Node _ tracker _ _) = Tuple.snd <$> tracker.inputs


subscribeOutput :: forall state is os m dout. (Record os -> dout) -> Node state is os m -> Signal dout
subscribeOutput fn node = fn <$> subscribeOutputs node


subscribeOutputs :: forall state is os m. Node state is os m -> Signal (Record os)
subscribeOutputs (Node _ tracker _ _) = Tuple.snd <$> tracker.outputs


subscribeState :: forall state is os m. Node state is os m -> Signal state
subscribeState (Node _ tracker _ _) = tracker.state


-- TODO: subscribeLastInput / subscribeLastOutput


-- TODO: connect

connect
    :: forall oA iB doutA dinB stateA stateB isA isB isB' osA osB osA' m
     . IsSymbol oA
    => IsSymbol iB
    => R.Cons oA doutA osA' osA
    => R.Cons iB dinB isB' isB
    => MonadEffect m
    => MonadRec m
    => Show dinB
    => Fn.Output oA
    -> Fn.Input iB
    -> (doutA -> dinB)
    -> Node stateA isA osA m
    -> Node stateB isB osB m
    -> m Unit
connect
    outputA
    inputB
    convert
    nodeA@(Node _ _ protocolA fnA)
    nodeB@(Node _ _ protocolB fnB) = do
    let subscription = subscribeOutput (Record.get outputA) nodeA
    testChan <- liftEffect $ Channel.channel "foooAAA"
    let

        logSignal = Channel.subscribe testChan ~> Console.log
        linkingSignal =
            subscription
            ~> convert
            ~> (\dout -> do
                    protocolB.modifyInputs
                        (\curInputs ->
                            (SingleInput $ Fn.inputId inputB) /\ Record.set inputB dout curInputs
                        )
                    Channel.send testChan $ show dout
                    -- with nodeB $ Process.sendIn inputB dout
                    -- liftEffect $ Console.log "-----<>-----"
                )
            -- ~> mempty
    -- with nodeB $ Process.lift $ liftEffect $ Signal.runSignal linkingSignal
                        -- Process.sendIn inputB dout
    -- traverse
    -- -- Channel.send
    -- liftEffect $ T.sequence_ linkingSignal
    liftEffect $ Signal.runSignal linkingSignal
    liftEffect $ Signal.runSignal logSignal
    liftEffect $ Channel.send testChan "BLAH"
    -- pure unit


{-
set :: forall state is os m. MonadEffect m => ( state /\ Record is /\ Record os ) -> Node state is os m -> m (Node state is os m)
set ( state /\ inputs /\ outputs ) node@(Node id protocolS fn) =
    pure node -- FIXME
-}


-- TODO: getAtInput, getAtOutput, updateInputs, updateOutputs, updateState ...



inputsShape :: forall state (is :: Row Type) os m g. RL.RowToList is g => Record.Keys g => Node state is os m -> List Fn.InputId
inputsShape (Node _ _ _ fn) = Fn.inputsShape fn


outputsShape :: forall state is (os :: Row Type) m g. RL.RowToList os g => Record.Keys g => Node state is os m -> List Fn.OutputId
outputsShape (Node _ _ _ fn) = Fn.outputsShape fn


-- TODO: mapRecord


shape
    :: forall state (is :: Row Type) (os :: Row Type) m g
     . RL.RowToList is g
    => RL.RowToList os g
    => Record.Keys g
    => Node state is os m
    -> List Fn.InputId /\ List Fn.OutputId
shape node = inputsShape node /\ outputsShape node


dimensions
    :: forall state is os m g
     . RL.RowToList is g
    => RL.RowToList os g
    => Record.Keys g
    => Node state is os m
    -> Int /\ Int
dimensions = shape >>> bimap List.length List.length


dimensionsBy
    :: forall state is os m g
     . RL.RowToList is g
    => RL.RowToList os g
    => Record.Keys g
    => (Fn.InputId -> Boolean)
    -> (Fn.OutputId -> Boolean)
    -> Node state is os m
    -> Int /\ Int
dimensionsBy iPred oPred = shape >>> bimap (List.filter iPred >>> List.length) (List.filter oPred >>> List.length)


{-
findInput :: forall i ii o oo state m d. (i -> Boolean) -> Fn state is os m -> Maybe (i /\ ii)
findInput pred (Fn _ inputs _ _) = Array.index inputs =<< Array.findIndex (Tuple.fst >>> pred) inputs


findOutput :: forall i ii o oo state m d. (o -> Boolean) -> Fn i ii o oo state m d -> Maybe (o /\ oo)
findOutput pred (Fn _ _ outputs _) = Array.index outputs =<< Array.findIndex (Tuple.fst >>> pred) outputs
-}


with :: forall state is os m. MonadEffect m => MonadRec m => Node state is os m -> ProcessM state is os m Unit -> m Unit
with (Node _ _ protocol fn) process =
    Fn.run' protocol $ Fn.cloneReplace fn process