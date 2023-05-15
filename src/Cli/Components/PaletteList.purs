module Cli.Components.PaletteList where

import Prelude

import Data.Array ((:), (!!))

import Blessed as B
import Blessed.Internal.BlessedSubj (Screen, ListBar, Box, List, Line, Button)
import Blessed.Internal.NodeKey (nk, NodeKey(..), type (<^>), RawNodeKey)
import Blessed.Internal.NodeKey as NodeKey

import Blessed.Internal.Core as Core
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.UI.Boxes.Box as Box
import Blessed.UI.Boxes.Box.Event as Box
import Blessed.UI.Boxes.Box.Method as Box
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Lists.List.Event as List
import Blessed.UI.Lists.List.Option as List
import Blessed.UI.Lists.List.Property as List


import Cli.Palette (Palette, palette)
import Cli.Palette (Item, fullInfo, toArray, pico8, hydraFns, x11colors, qitem, qitem') as Palette


paletteKey = (nk :: List <^> "palette")


pitemToListRow :: Palette.Item -> String
pitemToListRow item = "{" <> item.repr <> "-bg}      {/" <> item.repr <> "-bg} {" <> item.repr <> "-fg}" <> Palette.fullInfo item <> "{/" <> item.repr <> "-fg}"


selfNamedColors :: Array String
selfNamedColors = [ "red", "green", "blue", "yellow" ]


component :: forall state. Int -> Int -> Core.Blessed state
component left top =
    B.list paletteKey
        [ Box.width $ Dimension.percents 40.0
        , Box.height $ Dimension.percents 100.0
        , Box.top $ Offset.px top
        , Box.left $ Offset.px left
        , List.items $ pitemToListRow <$> (Palette.qitem "white" "title" : (Palette.qitem' <$> selfNamedColors) <> Palette.pico8 <> Palette.toArray palette <> Palette.hydraFns <> Palette.x11colors)
        , List.mouse true
        , List.keys true
        , Box.tags true
        , Box.scrollable true
        ]
        [ ]