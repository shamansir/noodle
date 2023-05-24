module Cli.Style where

import Prelude

import Cli.Palette (palette)
import Cli.Palette.Item (repr)


import Blessed.UI.Lists.List.Option as List
import Blessed.Core.EndStyle as ES
import Blessed.Core.ListStyle as LStyle


inletsOutlets =
    List.style
        [ LStyle.bg $ repr palette.background
        , LStyle.item
            [ ES.fg $ repr palette.itemNotSelected
            , ES.bg $ repr palette.background
            ]
        , LStyle.selected
            [ ES.fg $ repr palette.itemSelected
            , ES.bg $ repr palette.background
            ]
        ]