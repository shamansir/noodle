module Cli.Components.LoadFile where

import Prelude

import Effect (Effect)
import Control.Monad.Rec.Class (class MonadRec)
import Effect.Class (class MonadEffect, liftEffect)

import Effect.Console (log) as Console


import Type.Proxy (Proxy(..))

import Control.Monad.State (get, modify_) as State

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

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
import Noodle.Node2 as Node

import Toolkit.Hydra2 as Hydra
import Toolkit.Hydra2.Repr.Wrap (WrapRepr) as Hydra


import Noodle.Text.NetworkFile.Apply as File


handlers :: State -> Patch Hydra.State (Hydra.Instances Effect) -> Network Hydra.State (Hydra.Families Effect) (Hydra.Instances Effect) -> File.Handlers Hydra.State (Hydra.Instances Effect) Effect
handlers state patch (Network tk _) =
    { onNodeCreated : \(x /\ y) pHoldsNode -> do
        _ <- BlessedOp.runM state (
            Patch.withNode'
                pHoldsNode
                \patch_ node ->
                    pure unit
                    --NodeBox.fromNode "" patch (Id.familyRev $ Node.family node) node (tk :: Hydra.Toolkit Effect)
            )
        pure unit
    , onNodeCreated2 : \(x /\ y) nHoldsNode -> do
        _ <- BlessedOp.runM state (
                Node.withNode'
                        nHoldsNode
                        \node ->
                            pure unit
                            -- NodeBox.fromNode "" patch (Id.familyRev $ Node.family node) node (tk :: Hydra.Toolkit Effect)
                    )
        pure unit
    , onConnect : \link -> pure unit
    }


component
    :: forall m gstate (families :: Row Type) (instances :: Row Type) repr fsrl isrl
     . Id.ListsFamilies (Hydra.Families Effect) fsrl
    -- => RL.RowToList (Hydra.Instances Effect) isrl
    -- => Record.Keys isrl
    => Patch Hydra.State (Hydra.Instances Effect)
    -> Network Hydra.State (Hydra.Families Effect) (Hydra.Instances Effect)
    -> Core.Blessed State
component patch network =
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
                liftEffect $ do
                    -- TODO
                    _ <- File.applyFile Hydra.withFamily (Proxy :: _ Hydra.WrapRepr) patch network (handlers state patch network) []
                    pure unit
        ]
        []