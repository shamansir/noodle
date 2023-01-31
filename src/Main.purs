module Main where

import Prelude


import Effect (Effect)

import Cli.App as Cli

import Blessed ((>~))
import Blessed as B
import Blessed (exit) as Blessed

import Blessed.Core.Key as Key
import Blessed.Core.Offset as Offset
import Blessed.Core.FgBg as FgBg
import Blessed.Core.Border as Border
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Style as Style
import Blessed.Internal.BlessedSubj (Screen, ListBar, Box, List)
import Blessed.Internal.NodeKey (NodeKey(..), type (<^>))


import Blessed.UI.Boxes.Box as Box
import Blessed.UI.Base.Screen as Screen
import Blessed.UI.Base.Screen.Option as Screen
import Blessed.UI.Base.Screen.Event as Screen
import Blessed.UI.Base.Screen.Method as Screen
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Event as Box
import Blessed.UI.Boxes.Box.Method as Box
import Blessed.UI.Lists.ListBar.Option as ListBar


mainScreen = NodeKey :: Screen <^> "main-scr"
patchesBar = NodeKey :: ListBar <^> "patches-bar"
patchBox = NodeKey :: Box <^> "patch-box"
nodeList = NodeKey :: List <^> "node-list"


main :: Effect Unit
main = do
  Cli.run
    (B.screenAnd mainScreen

        [ Screen.title "foo"
        , Screen.smartCSR true
        ]

        [ B.listbar patchesBar
            [ Box.top $ Offset.px 0 ]
            []
        , B.box patchBox
            []
            $ []
        , B.listAnd nodeList
            []
            []
            \_ ->
                pure unit
        ]

        $ \screen ->
            pure unit
        )
