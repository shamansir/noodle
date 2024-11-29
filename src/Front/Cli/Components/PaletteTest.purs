module Cli.Components.PaletteTest where

import Prelude

import Blessed as B
import Blessed ((>~))
import Blessed.Core.Key (alpha, control, escape) as Key
import Blessed.Core.Coord ((<+>))
import Blessed.Core.Coord as Coord
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset
import Blessed.Internal.Core as Core
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.Internal.BlessedSubj (Box, Screen)
import Blessed.Internal.NodeKey (nk, type (<^>))
import Blessed.UI.Base.Screen.Event (key) as Screen
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Base.Screen.Option (fullUnicode, smartCSR, title) as Screen


import Noodle.Ui.Cli.Tagging.At as T
import Noodle.Ui.Cli.Tagging.At (ChannelLabel) as At
import Noodle.Ui.Cli.Palette.Mark (class Mark)


type MainScreenKey = Screen <^> "main"
type PaletteTestKey = Box <^> "palette-test"


mainScreenKey  = nk :: MainScreenKey
paletteTestKey = nk :: PaletteTestKey



paletteBox
    :: Core.Blessed Unit
paletteBox =
    B.boxAnd paletteTestKey

        [ Box.top $ Offset.calc $ Coord.center <+> Coord.px 1
        , Box.left $ Offset.center
        , Box.width $ Dimension.percents 100.0
        , Box.height $ Dimension.percents 100.0
        , Box.content "PALETTE"
        , Box.tags true
        ]

        [
        ]

        $ \_ -> do pure unit


component
    :: Core.Blessed Unit
component =
    B.screenAnd mainScreenKey

        [ Screen.title "Noodle"
        , Screen.smartCSR true
        , Screen.fullUnicode true
        , Screen.key
            [ Key.escape, Key.alpha 'q', (Key.control $ Key.alpha 'C') ]
            $ \_ kevt -> B.exit
        ]

        [ paletteBox
        ]

        $ \_ -> do
            mainScreenKey >~ Screen.render
