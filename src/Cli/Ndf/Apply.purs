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
import Noodle.Network (Network(..))
import Noodle.Network as Network
import Noodle.Patch as Patch
import Noodle.Patch (Patch)
import Noodle.Patch.Has as PHas
import Noodle.Patch.Is as PIs
import Noodle.Node as Node
import Noodle.Node (Node)
import Noodle.Node.MapsFolds.Repr (class ToReprFoldToMapsHelper, class ToReprHelper) as R
import Noodle.Toolkit (Toolkit)
import Noodle.Node.HoldsNodeState (class IsNodeState, fromGlobal)
import Noodle.Stateful (setM, get) as Stateful

import Tookit.Hydra (Families, Instances, State, Toolkit, withFamily) as Hydra
import Tookit.Hydra.Repr.Wrap (WrapRepr) as Hydra
import Tookit.Hydra.Family.Render.Cli (CliF) as Hydra

import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile.Apply as File
import Noodle.Text.NdfFile.Command as Cmd
import Noodle.Text.NdfFile.Command as C


addNodeBox
    :: forall f rlins instances' state is os isrl osrl repr_is repr_os
     . PIs.IsReprableRenderableNodeInPatch Hydra.CliF Hydra.State instances' (Hydra.Instances Effect) rlins f state is os isrl osrl repr_is repr_os Hydra.WrapRepr Effect
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
    , onConnect : \def linkId link ->
        BlessedOp.runM' stateRef $ do
            state <- State.get
            case (/\) <$> Map.lookup def.onode state.nodeKeysMap <*> Map.lookup def.inode state.nodeKeysMap of
                Just (onodeKey /\ inodeKey) -> do
                    linkCmp <- Link.create
                                linkId
                                { key : onodeKey, id : def.onode }
                                (OutputIndex def.outputIndex)
                                { key: inodeKey, id : def.inode }
                                (InputIndex def.inputIndex)
                    State.modify_ $ Link.store linkCmp
                    Key.patchBox >~ Link.append linkCmp
                    logNdfCommandM $ Cmd.Connect (C.nodeId $ reflect' def.onode) (C.outputIndex def.outputIndex) (C.nodeId $ reflect' def.inode) (C.inputIndex def.inputIndex)
                    Key.mainScreen >~ Screen.render
                    pure unit
                Nothing -> pure unit
    , onConnect2 : \def linkId link ->
        BlessedOp.runM' stateRef $ do
            state <- State.get
            case (/\) <$> Map.lookup def.onode state.nodeKeysMap <*> Map.lookup def.inode state.nodeKeysMap of
                Just (onodeKey /\ inodeKey) -> do
                    linkCmp <- Link.create
                                linkId
                                { key : onodeKey, id : def.onode }
                                (OutputIndex def.outputIndex)
                                { key: inodeKey, id : def.inode }
                                (InputIndex def.inputIndex)
                    State.modify_ $ Link.store linkCmp
                    Key.patchBox >~ Link.append linkCmp
                    logNdfCommandM $ Cmd.Connect (C.nodeId $ reflect' def.onode) (C.outputIndex def.outputIndex) (C.nodeId $ reflect' def.inode) (C.inputIndex def.inputIndex)
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