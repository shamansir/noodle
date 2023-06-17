module Cli.Components.LoadFile where

import Prelude

import Effect (Effect)
import Control.Monad.Rec.Class (class MonadRec)
import Effect.Class (class MonadEffect, liftEffect)

import Effect.Console (log) as Console


import Type.Proxy (Proxy(..))

import Control.Monad.State (get, modify_) as State

import Data.Maybe (Maybe(..))
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\))
import Data.Repr (class FromToReprRow, class ToReprRow)
import Record.Extra as Record
import Type.RowList as RL

import Blessed as B
import Blessed ((>~))

import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Style as Style
import Blessed.Internal.Core as Core
import Blessed.Internal.BlessedOp (BlessedOp, BlessedOpM)
import Blessed.Internal.BlessedOp as BlessedOp


import Blessed.UI.Boxes.Box.Option (content, height, left, style, top, width) as Box

import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Screen.Method (render) as Screen

import Cli.Keys as Key
import Cli.Palette as Palette
import Cli.State (State)
import Cli.State (patchIdFromIndex) as State
import Cli.State.NwWraper (unwrapN, withNetwork)
import Cli.Components.NodeBox as NodeBox
import Cli.Style as Style

import Noodle.Id as Id
import Noodle.Network2 (Network(..))
import Noodle.Network2 as Network
import Noodle.Patch4 as Patch
import Noodle.Patch4 (Patch)
import Noodle.Patch4.Has as Has
import Noodle.Node2 as Node
import Noodle.Node2 (Node)
import Noodle.Node2.MapsFolds.Repr
    ( class ToReprHelper, class ToReprFoldToMapsHelper
    , Repr(..)
    , nodeToRepr, nodeToMapRepr
    , subscribeReprChanges, subscribeReprMapChanges
    ) as R
import Noodle.Toolkit3 (Toolkit)
import Noodle.Toolkit3.Has (class HasNodesOf) as Toolkit

import Toolkit.Hydra2 as Hydra
import Toolkit.Hydra2.Repr.Wrap (WrapRepr) as Hydra

import Unsafe.Coerce (unsafeCoerce)
import Noodle.Text.NetworkFile.Apply as File
import Noodle.Text.NetworkFile.Command as Cmd

import Cli.State.NwWraper (unwrapN, withNetwork)


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
    => Toolkit Hydra.State (Hydra.Families Effect)
    -> Patch Hydra.State (Hydra.Instances Effect)
    -> Node f state is os Effect
    -> BlessedOpM State Effect _
addNodeBox tk patch node =
    NodeBox.fromNode "" patch (Id.familyRev $ Node.family node) node


handlers
    :: State
    -> Patch Hydra.State (Hydra.Instances Effect)
    -> Network Hydra.State (Hydra.Families Effect) (Hydra.Instances Effect)
    -> File.Handlers Hydra.State (Hydra.Instances Effect) Effect Hydra.WrapRepr
handlers state patch (Network tk _) =
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
            (pHoldsNode :: Patch.HoldsNodeMRepr Hydra.State (Hydra.Instances Effect) Effect Hydra.WrapRepr)
            (\patch node ->
                BlessedOp.runM state (addNodeBox (tk :: Hydra.Toolkit Effect) patch node)
            )
        {-
        _ <- BlessedOp.runM state (
            Patch.withNodeMRepr
                (pHoldsNode :: Patch.HoldsNodeMRepr Hydra.State (Hydra.Instances Effect) Effect Hydra.WrapRepr)
                (addNodeBox (tk :: Hydra.Toolkit Effect))
            ) -}
        pure unit
    , onConnect : \link -> pure unit
    }


component ::
    {-}:: forall fsrl
     . Id.ListsFamilies (Hydra.Families Effect) fsrl
    -- => RL.RowToList (Hydra.Instances Effect) isrl
    -- => Record.Keys isrl
    => Patch Hydra.State (Hydra.Instances Effect)
    -> Network Hydra.State (Hydra.Families Effect) (Hydra.Instances Effect)
    -> -} Core.Blessed State
component =
    B.button Key.addPatchButton
        [ Box.content "L"
        , Box.top $ Offset.px 0
        , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 3
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Button.mouse true
        , Style.addPatch
        , Core.on Button.Press
            \_ _ -> do
                state <- State.get
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
                                    (handlers state patch network)
                                    [
                                        Cmd.MakeNode "osc" 40 60 "osc-1"
                                    ]
                            pure unit
                    Nothing -> pure unit
        ]
        []