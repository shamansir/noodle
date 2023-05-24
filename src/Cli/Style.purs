module Cli.Style where

import Prelude

import Cli.Palette as Palette


import Blessed.UI.Lists.List.Option as List
import Blessed.Core.EndStyle as ES
import Blessed.Core.ListStyle as LStyle


inletsOutlets =
    List.style
        [ LStyle.bg Palette.background'
        , LStyle.item
            [ ES.fg Palette.itemNotSelected'
            , ES.bg Palette.background'
            ]
        , LStyle.selected
            [ ES.fg Palette.itemSelected'
            , ES.bg Palette.background'
            ]
        ]