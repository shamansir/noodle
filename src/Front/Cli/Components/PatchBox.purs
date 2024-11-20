module Cli.Components.PatchBox where

import Prelude

import Blessed as B
import Blessed.Core.Border (type_, _line) as Border
import Blessed.Core.Coord ((<+>), (<->))
import Blessed.Core.Coord as Coord
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.Core as Core
import Blessed.UI.Boxes.Box.Option as Box
import Cli.Components.Library as Library
import Cli.Keys as Key
import Cli.State (State)
import Cli.Style (patchBox, patchBoxBorder) as Style
import Data.Lens (united)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Noodle.Id as Id

-- import Cli.Components.InputIndicator as InputIndicator
-- import Cli.Components.OutputIndicator as OutputIndicator


component :: forall tk p fs r m. Array Id.FamilyR -> Core.Blessed (State tk p fs r m)
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