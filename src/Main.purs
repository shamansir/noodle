module Main where

import Prelude


import Effect (Effect)

import Cli.App as Cli

import Blessed ((>~))
import Blessed as B
import Blessed (exit) as Blessed
import Type.Proxy (Proxy(..))

import Blessed.Core.Key as Key
import Blessed.Core.Offset as Offset
import Blessed.Core.FgBg as FgBg
import Blessed.Core.Border as Border
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Style as Style
import Blessed.Core.Coord ((<+>))
import Blessed.Core.Coord as Coord
import Blessed.Core.ListStyle as LStyle

import Blessed.Internal.BlessedSubj (Screen, ListBar, Box, List)
import Blessed.Internal.NodeKey (nk, NodeKey(..), type (<^>))
import Blessed.Internal.NodeKey as NodeKey


import Blessed.UI.Boxes.Box as Box
import Blessed.UI.Base.Screen as Screen
import Blessed.UI.Base.Screen.Option as Screen
import Blessed.UI.Base.Screen.Event as Screen
import Blessed.UI.Base.Screen.Method as Screen
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Event as Box
import Blessed.UI.Boxes.Box.Method as Box
import Blessed.UI.Lists.List.Option as List


mainScreen = nk :: Screen <^> "main-scr"
patchesBar = nk :: ListBar <^> "patches-bar"
patchBox = nk :: Box <^> "patch-box"
nodeList = nk :: List <^> "node-list"
nodeBox = nk :: Box <^> "node-box"
inlets = nk :: ListBar <^> "inlets"
outlets = nk :: ListBar <^> "outlets"


palette =
    { background : "#111" -- 0
    , itemNotSelected : "#006600" -- 1
    , itemSelected : "#00ff00" -- 2
    , border : "#f0f0f0" -- 3
    , nodeListFg : "#666" -- 4
    , nodeListSelFg : "white" -- 5
    , nodeBoxBorder : "blue" -- 6
    , familyMarker : "#000033" -- 7
    , linkColor : "green" -- 8
    , focusedBorder : "white"
    , foreground : "white"
    , background2 : "black"
    }


patches = [ "Patch1", "Patch2", "+" ]

items = [ "foo", "bar", "ololo", "hello", "foo1", "bar1", "ololo1", "hello1", "foo2", "bar2", "ololo2", "hello2" ]


main :: Effect Unit
main = do
  Cli.run
    (B.screenAnd mainScreen

        [ Screen.title "Noodle"
        , Screen.smartCSR true
        , Screen.fullUnicode true
        ]

        [ B.listbar patchesBar
            [ Box.top $ Offset.px 0
            , Box.left $ Offset.px 0
            , Box.width $ Dimension.percents 100.0
            , Box.height $ Dimension.px 1
            , List.mouse true
            , List.items patches
            , List.style
                [ LStyle.bg palette.background
                , LStyle.item
                    [ FgBg.fg palette.itemNotSelected
                    , FgBg.bg palette.background
                    ]
                , LStyle.selected
                    [ FgBg.fg palette.itemSelected
                    , FgBg.bg palette.background
                    ]
                ]
            ]
            []

        , B.box patchBox
            [ Box.top $ Offset.calc $ Coord.center <+> Coord.px 1
            , Box.left $ Offset.center
            , Box.width $ Dimension.percents 100.0
            , Box.height $ Dimension.calc $ Coord.percents 100.0 <+> Coord.px 1
            , Box.content "Patch"
            , Box.tags true
            , Box.border [ Border.type_ Border._line ]
            , Box.style
                [ Style.fg palette.foreground
                , Style.bg palette.background2
                , Style.border [ Border.fg palette.border ]
                ]
            ]
            []

        , B.listAnd nodeList
            [ Box.top $ Offset.px 0
            , Box.left $ Offset.px 0
            , Box.width $ Dimension.px 0
            , Box.height $ Dimension.percents 40.0
            , Box.draggable true
            , Box.scrollable true
            , List.items items
            , List.mouse true
            , List.keys true
            , Box.border [ Border.type_ Border._line, Border.fg palette.nodeListFg ]
            ]
            []
            \_ ->
                pure unit
        ]

        $ \screen ->
            pure unit
        )
