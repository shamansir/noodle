module Cli.Components.LoadFileButton where

import Prelude

import Effect (Effect)
import Control.Monad.Rec.Class (class MonadRec)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Effect.Console (log) as Console


import Type.Proxy (Proxy(..))
import Data.SProxy (reflect')

import Control.Monad.State (get, modify_) as State

import Data.Maybe (Maybe(..))
import Data.Tuple (snd) as Tuple
import Data.Map as Map
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
import Cli.State (State, OutputIndex(..), InputIndex(..), logNdfCommandM)
import Cli.State (patchIdFromIndex) as State
import Cli.State.NwWraper (unwrapN, withNetwork)
import Cli.Components.NodeBox as NodeBox
import Cli.Components.Link as Link
import Cli.Components.NodeBox.HasBody (class HasBody', run', class HasCustomSize)
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
import Noodle.Node2.HoldsNodeState (class IsNodeState, fromGlobal)
import Noodle.Stateful (setM, get) as Stateful

import Toolkit.Hydra2 as Hydra
import Toolkit.Hydra2.Repr.Wrap (WrapRepr) as Hydra
import Toolkit.Hydra2.Family.Render.Cli (CliD, CliF) as Hydra

import Unsafe.Coerce (unsafeCoerce)
import Noodle.Text.NdfFile (toNdfCode, from) as NdfFile
import Noodle.Text.NdfFile.Apply as File
import Noodle.Text.NdfFile.Command as Cmd
import Noodle.Text.NdfFile.Command as C

import Cli.State.NwWraper (unwrapN, withNetwork)
import Cli.Ndf.Apply (apply) as NdfFile


component ::
    {-}:: forall fsrl
     . Id.ListsFamilies (Hydra.Families Effect) fsrl
    -- => RL.RowToList (Hydra.Instances Effect) isrl
    -- => Record.Keys isrl
    => Patch Hydra.State (Hydra.Instances Effect)
    -> Network Hydra.State (Hydra.Families Effect) (Hydra.Instances Effect)
    -> -} Core.Blessed State
component =
    B.button Key.loadFileButton
        [ Box.content "L"
        , Box.top $ Offset.px 0
        , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 3
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Button.mouse true
        , Style.menuButton
        , Core.on Button.Press
            \_ _ ->
                NdfFile.apply $ NdfFile.from
                                    "hydra" 0.1
                                    [ Cmd.MakeNode (C.family "osc") (C.coord 25) (C.coord 10) (C.nodeId "osc-1")
                                    , Cmd.MakeNode (C.family "pi") (C.coord 22) (C.coord 2) (C.nodeId "pi-1")
                                    , Cmd.Connect (C.nodeId "pi-1") (C.outputIndex 0) (C.nodeId "osc-1") (C.inputIndex 0)
                                    , Cmd.Send (C.nodeId "osc-1") (C.inputIndex 1) (C.encodedValue "V N 20.0")
                                    , Cmd.Send (C.nodeId "osc-1") (C.inputIndex 2) (C.encodedValue "V N 40.0")
                                    ]
        ]
        []