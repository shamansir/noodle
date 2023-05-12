module Cli.Style where

import Prelude

import Cli.Palette (palette)


import Blessed.UI.Lists.List.Option as List
import Blessed.Core.EndStyle as ES
import Blessed.Core.ListStyle as LStyle


inletsOutlets =
    List.style
        [ LStyle.bg palette.background
        , LStyle.item
            [ ES.fg palette.itemNotSelected
            , ES.bg palette.background
            ]
        , LStyle.selected
            [ ES.fg palette.itemSelected
            , ES.bg palette.background
            ]
        ]