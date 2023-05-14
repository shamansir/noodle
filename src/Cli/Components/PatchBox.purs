module Cli.Components.PatchBox where

import Prelude


import Blessed as B

import Blessed.UI.Boxes.Box.Option as Box

import Blessed.Core.Border (type_, _line, fg) as Border
import Blessed.Core.Coord ((<+>), (<->))
import Blessed.Core.Coord as Coord
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.Core.Style as Style

import Cli.Palette (palette)
import Cli.Keys as Key

import Cli.Components.Library as Library

import Noodle.Id as Id


component :: Array Id.FamilyR -> _
component families =
    B.box Key.patchBox

        [ Box.top $ Offset.calc $ Coord.center <+> Coord.px 1
        , Box.left $ Offset.center
        , Box.width $ Dimension.percents 100.0
        , Box.height $ Dimension.calc $ Coord.percents 100.0 <-> Coord.px 1
        , Box.content "Patch"
        , Box.tags true
        , Box.border
            [ Border.type_ Border._line
            ]
        , Box.style
            [ Style.fg palette.foreground
            , Style.bg palette.background2
            , Style.border [ Border.fg palette.border ]
            ]
        ]

        [ Library.component families
        ]