module Cli.Components.NodeBox.RemoveButton where

import Prelude


import Blessed as B

import Blessed ((>~))
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset (Offset)
import Blessed.Core.Offset as Offset
import Blessed.Core.Coord as Coord
import Blessed.Core.Coord ((<->))
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Tagger as T
import Blessed.Tagger ((<:>))

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Method (setContent) as Box
import Blessed.Internal.Core as Core
import Blessed.UI.Forms.Button.Option (mouse) as Button
import Blessed.UI.Forms.Button.Event (ButtonEvent(..)) as Button
import Blessed.UI.Base.Element.Event (ElementEvent(..)) as Element

import Cli.Keys (RemoveButtonKey)
import Cli.Style as Style


component :: forall state. RemoveButtonKey -> Core.Blessed state
component buttonKey =
    B.button buttonKey
        [ Box.content "."
        , Box.top $ Offset.px $ -1
        , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 3
        -- , Box.left $ Offset.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.width $ Dimension.px 1
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Button.mouse true
        , Style.inletsOutlets
        , Core.on Button.Press
            \_ _ -> pure unit --TODO
        , Core.on Element.MouseOver
            \_ _ -> buttonKey >~ Box.setContent "x"
        , Core.on Element.MouseOut
            \_ _ -> buttonKey >~ Box.setContent "."
        ]
        []