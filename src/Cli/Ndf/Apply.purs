module Cli.Ndf.Apply where

import Prelude


import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)


import Type.Proxy (Proxy(..))
import Data.SProxy (reflect')

import Control.Monad.State (get, modify_) as State

import Data.Maybe (Maybe(..))
import Data.Tuple (snd) as Tuple
import Data.Map as Map
import Data.Tuple.Nested ((/\), type (/\))
import Data.Repr (class FromToReprRow)

import Blessed ((>~))

import Blessed.Internal.BlessedOp (BlessedOpM)
import Blessed.Internal.BlessedOp as BlessedOp


import Blessed.UI.Base.Screen.Method (render) as Screen

import Cli.Keys as Key
import Cli.State (State, OutputIndex(..), InputIndex(..), logNdfCommandM)
import Cli.State.NwWraper (unwrapN)
import Cli.Components.NodeBox as NodeBox
import Cli.Components.Link as Link
import Cli.Components.NodeBox.HasBody (class HasBody', class HasCustomSize)

import Noodle.Id as Id
import Noodle.Network2 (Network(..))
import Noodle.Network2 as Network
import Noodle.Patch4 as Patch
import Noodle.Patch4 (Patch)
import Noodle.Patch4.Has as Has
import Noodle.Node2 as Node
import Noodle.Node2 (Node)
import Noodle.Node2.MapsFolds.Repr (class ToReprFoldToMapsHelper, class ToReprHelper) as R
import Noodle.Toolkit3 (Toolkit)
import Noodle.Node2.HoldsNodeState (class IsNodeState, fromGlobal)
import Noodle.Stateful (setM, get) as Stateful

import Toolkit.Hydra2 (Families, Instances, State, Toolkit, withFamily) as Hydra
import Toolkit.Hydra2.Repr.Wrap (WrapRepr) as Hydra
import Toolkit.Hydra2.Family.Render.Cli (CliF) as Hydra

import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile.Apply as File
import Noodle.Text.NdfFile.Command as Cmd
import Noodle.Text.NdfFile.Command as C


addNodeBox
    :: forall f instances' state is os isrl osrl repr_is repr_os
     . Has.HasInstancesOf f instances' (Hydra.Instances Effect) (Array (Node f state is os Effect))
    -- => RL.RowToList (Hydra.Instances Effect) rli
    -- => Record.Keys rli
    => Id.HasInputsAt is isrl
    => Id.HasOutputsAt os osrl
    => R.ToReprHelper Effect f is isrl os osrl repr_is repr_os Hydra.WrapRepr state
    => R.ToReprFoldToMapsHelper f is isrl os osrl Hydra.WrapRepr state
    => FromToReprRow isrl is Hydra.WrapRepr
    => FromToReprRow osrl os Hydra.WrapRepr
    => Node.NodeBoundKeys Node.I isrl Id.Input f state is os Effect (Node.HoldsInputInNodeMRepr Effect Hydra.WrapRepr)
    => Node.NodeBoundKeys Node.O osrl Id.Output f state is os Effect (Node.HoldsOutputInNodeMRepr Effect Hydra.WrapRepr)
    => HasBody' (Hydra.CliF f) (Node f state is os Effect) state Effect
    => HasCustomSize (Hydra.CliF f) (Node f state is os Effect)
    => IsNodeState Hydra.State state
    => Int /\ Int
    -> Toolkit Hydra.State (Hydra.Families Effect)
    -> Patch Hydra.State (Hydra.Instances Effect)
    -> Node f state is os Effect
    -> BlessedOpM State Effect _
addNodeBox pos tk patch node = do
    let (mbState :: Maybe state) = fromGlobal $ Stateful.get patch -- TODO: make a signal of global state changes and update node's state using subscription
    node' <- liftEffect $ case mbState of
        Just state -> Stateful.setM state node
        Nothing -> pure node
    NodeBox.fromNodeAt pos "" patch (Id.familyRev $ Node.family node) node'


handlers
    :: Ref State
    -> Patch Hydra.State (Hydra.Instances Effect)
    -> Network Hydra.State (Hydra.Families Effect) (Hydra.Instances Effect)
    -> File.Handlers Hydra.CliF Hydra.State (Hydra.Instances Effect) Effect Hydra.WrapRepr
handlers stateRef patch (Network tk _) =
    { onNodeCreated : \(x /\ y) pHoldsNode -> do
        -- _ <- BlessedOp.runM state (
        --     Patch.withNode'
        --         (pHoldsNode :: Patch.HoldsNode' Hydra.State (Hydra.Instances Effect) Effect)
        --         (addNodeBox (tk :: Hydra.Toolkit Effect))
        --     )
        pure unit
    , onNodeCreated2 : \(x /\ y) nHoldsNode -> do
        -- _ <- BlessedOp.runM state (
        --         Node.withNode'
        --                 nHoldsNode
        --                 \node ->
        --                     --pure unit
        --                     addNodeBox (tk :: Hydra.Toolkit Effect) patch node
        --                     -- NodeBox.fromNode "" patch (Id.familyRev $ Node.family node) node
        --             )
        pure unit
    , onNodeCreated3 : \(x /\ y) pHoldsNode -> do
        _ <- Patch.withNodeMRepr
            (pHoldsNode :: Patch.HoldsNodeMRepr Hydra.CliF Hydra.State (Hydra.Instances Effect) Effect Hydra.WrapRepr)
            (\patch node ->
                BlessedOp.runM' stateRef (addNodeBox (x /\ y) (tk :: Hydra.Toolkit Effect) patch node)
            )
        {-
        _ <- BlessedOp.runM state (
            Patch.withNodeMRepr
                (pHoldsNode :: Patch.HoldsNodeMRepr Hydra.State (Hydra.Instances Effect) Effect Hydra.WrapRepr)
                (addNodeBox (tk :: Hydra.Toolkit Effect))
            ) -}
        pure unit
    , onConnect : \(onode /\ inode) (outputIdx /\ inputIdx) link ->
        BlessedOp.runM' stateRef $ do
            state <- State.get
            case (/\) <$> Map.lookup onode state.nodeKeysMap <*> Map.lookup inode state.nodeKeysMap of
                Just (onodeKey /\ inodeKey) -> do
                    linkCmp <- Link.create
                                { key : onodeKey, id : onode }
                                (OutputIndex outputIdx)
                                { key: inodeKey, id : inode }
                                (InputIndex inputIdx)
                    State.modify_ $ Link.store linkCmp
                    Key.patchBox >~ Link.append linkCmp
                    logNdfCommandM $ Cmd.Connect (C.nodeId $ reflect' onode) (C.outputIndex outputIdx) (C.nodeId $ reflect' inode) (C.inputIndex inputIdx)
                    Key.mainScreen >~ Screen.render
                    pure unit
                Nothing -> pure unit
    , onConnect2 : \(onode /\ inode) (outputIdx /\ inputIdx) (outputId /\ inputId) link ->
        BlessedOp.runM' stateRef $ do
            state <- State.get
            case (/\) <$> Map.lookup onode state.nodeKeysMap <*> Map.lookup inode state.nodeKeysMap of
                Just (onodeKey /\ inodeKey) -> do
                    linkCmp <- Link.create
                                { key : onodeKey, id : onode }
                                (OutputIndex outputIdx)
                                { key: inodeKey, id : inode }
                                (InputIndex inputIdx)
                    State.modify_ $ Link.store linkCmp
                    Key.patchBox >~ Link.append linkCmp
                    logNdfCommandM $ Cmd.Connect (C.nodeId $ reflect' onode) (C.outputIndex outputIdx) (C.nodeId $ reflect' inode) (C.inputIndex inputIdx)
                    Key.mainScreen >~ Screen.render
                    pure unit
                Nothing -> pure unit
    }


apply :: forall m. MonadEffect m => NdfFile -> BlessedOpM State m Unit
apply file = do
    state <- State.get
    stateRef <- BlessedOp.getStateRef -- FIXME: use `Blessed.impairN` for handlers
    let network = unwrapN state.network
    let mbCurrentPatchId = Tuple.snd <$> state.currentPatch
    let mbCurrentPatch = mbCurrentPatchId >>= \id -> Network.patch id network
    case mbCurrentPatch of
        Just patch ->
            liftEffect $ do
                -- TODO
                _ <- File.applyFile
                        Hydra.withFamily
                        (Proxy :: _ Hydra.WrapRepr)
                        patch
                        network
                        (handlers stateRef patch network)
                        file
                pure unit
        Nothing -> pure unit