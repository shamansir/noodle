module Cli.Components.NodeBox.InfoBox where

import Prelude

import Blessed as B

import Blessed.Internal.Core (Blessed) as Core
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset


import Blessed.UI.Boxes.Box.Option as Box

import Cli.Keys (InfoBoxKey)
import Cli.Style as Style

component:: forall state. InfoBoxKey -> Int → Core.Blessed state
component nextInfoBox boxWidth =
    B.box nextInfoBox
        [ Box.top $ Offset.px 1
        , Box.left $ Offset.px 0
        -- , Box.width $ Dimension.calc $ C.px boxWidth <-> C.px 2 -- FIXME: didn't work
        , Box.width $ Dimension.px $ boxWidth - 2
        , Box.height $ Dimension.px 1
        , Box.tags true
        , Box.content ""
        , Style.infoBox
        ]
        [ ]