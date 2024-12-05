module Cli.Components.PatchBox where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Type.Proxy (Proxy(..))

import Blessed as B
import Blessed.Core.Border (type_, _line) as Border
import Blessed.Core.Coord ((<+>), (<->))
import Blessed.Core.Coord as Coord
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.Core as Core
import Blessed.UI.Boxes.Box.Option as Box

import Cli.Keys as Key
import Cli.State (State)
import Cli.Style (patchBox, patchBoxBorder) as Style
import Cli.Components.Library as Library
import Cli.Class.CliFriendly (class CliFriendly)

import Noodle.Wiring (class Wiring)
import Noodle.Id as Id
import Noodle.Repr (class HasFallback)
import Noodle.Toolkit (Toolkit, class MarkToolkit)
import Noodle.Toolkit (class HoldsFamilies) as Toolkit
import Noodle.Fn.ToFn (class PossiblyToFn)
import Noodle.Ui.Cli.Tagging.At as T
import Noodle.Ui.Cli.Tagging.At (ChannelLabel) as At

-- import Cli.Components.InputIndicator as InputIndicator
-- import Cli.Components.OutputIndicator as OutputIndicator


component
    :: forall tk p fs repr
     . HasFallback repr
    => PossiblyToFn tk (Maybe repr) (Maybe repr) Id.FamilyR
    => Toolkit.HoldsFamilies repr Effect fs
    => CliFriendly tk fs repr Effect
    => Toolkit tk fs repr Effect
    -> Core.Blessed (State tk p fs repr Effect)
component toolkit =
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

        [ Library.component toolkit
        -- , InputIndicator.component
        -- , OutputIndicator.component
        ]

        $ \_ -> do
            pure unit
            {-
            let mbEditor = Hydra.createEditorFor (H.Value $ H.Number 0.0) $ const $ pure unit
            case mbEditor of
                Just (_ /\ editor) -> do
                    editor
                Nothing -> pure unit
                    -- Key.patchBox ~> Node.append editor
            -}