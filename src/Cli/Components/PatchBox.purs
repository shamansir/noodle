module Cli.Components.PatchBox where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Blessed as B

import Blessed.Internal.Core as Core

import Blessed.UI.Boxes.Box.Option as Box

import Blessed.Core.Border (type_, _line) as Border
import Blessed.Core.Coord ((<+>), (<->))
import Blessed.Core.Coord as Coord
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.Internal.BlessedOp (BlessedOp)

import Cli.Keys as Key
import Cli.State (State)
import Cli.Components.Library as Library
import Cli.Style (patchBox, patchBoxBorder) as Style

import Noodle.Id as Id

import Toolkit.Hydra.Types as H
import Toolkit.Hydra.Repr.Wrap as H
import Toolkit.Hydra.Family.Render.Cli (createEditorFor) as Hydra

import Cli.Components.InputIndicator as InputIndicator
import Cli.Components.OutputIndicator as OutputIndicator


component :: Array Id.FamilyR -> Core.Blessed State
component families =
    B.boxAnd Key.patchBox

        [ Box.top $ Offset.calc $ Coord.center <+> Coord.px 1
        , Box.left $ Offset.center
        , Box.width $ Dimension.percents 100.0
        , Box.height $ Dimension.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.content "Patch"
        , Box.tags true
        , Style.patchBox
        , Style.patchBoxBorder
        ]

        [ Library.component families
        , InputIndicator.component
        , OutputIndicator.component
        ]

        $ \_ -> do
            let mbEditor = Hydra.createEditorFor (H.Value $ H.Number 0.0) $ const $ pure unit
            case mbEditor of
                Just (_ /\ editor) -> do
                    editor
                Nothing -> pure unit
                    -- Key.patchBox ~> Node.append editor
