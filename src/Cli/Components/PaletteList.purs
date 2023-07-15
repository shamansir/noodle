module Cli.Components.PaletteList where

import Prelude

import Data.Array ((:))
import Blessed.Tagger (s, bg ,fg)
import Blessed.Tagger (render) as Tags

import Blessed as B
import Blessed.Internal.BlessedSubj (List)
import Blessed.Internal.NodeKey (type (<^>), nk)

import Blessed.Internal.Core as Core
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Lists.List.Option (items, keys, mouse) as List


import Cli.Palette.Item (Item, fullInfo, qitem, qitem') as Palette
import Cli.Palette.Set.Hydra (hydraFns) as Palette
import Cli.Palette.Set.Pico8 (pico8) as Palette
import Cli.Palette.Set.X11 (x11colors) as Palette
import Cli.Palette (asArray) as Palette
import Cli.Tagging as T


paletteKey = (nk :: List <^> "palette")


pitemToListRow :: Palette.Item -> String
pitemToListRow =
    Tags.render <<< T.paletteItem


selfNamedColors :: Array String
selfNamedColors = [ "red", "green", "blue", "yellow" ]


component :: forall state. Int -> Int -> Number -> Number -> Core.Blessed state
component left top width height =
    B.list paletteKey
        [ Box.width $ Dimension.percents width
        , Box.height $ Dimension.percents height
        , Box.top $ Offset.px top
        , Box.left $ Offset.px left
        , List.items $ pitemToListRow <$> (Palette.qitem "white" "title" : (Palette.qitem' <$> selfNamedColors) <> Palette.pico8 <> Palette.asArray <> Palette.hydraFns <> Palette.x11colors)
        , List.mouse true
        , List.keys true
        , Box.tags true
        , Box.scrollable true
        ]
        [ ]