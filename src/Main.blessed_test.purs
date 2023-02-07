module MainBlessedTest where

import Prelude


import Effect (Effect)

import Cli.App as Cli

import Blessed ((>~))
import Blessed as B
import Blessed (exit) as Blessed

import Blessed.Core.Key as Key
import Blessed.Core.Offset as Offset
import Blessed.Core.EndStyle as ES
import Blessed.Core.Border as Border
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Style as Style
import Blessed.Internal.BlessedSubj (Screen, ListBar, Box, List)
import Blessed.Internal.NodeKey (NodeKey(..), type (<^>), nk)


import Blessed.UI.Boxes.Box as Box
import Blessed.UI.Base.Screen as Screen
import Blessed.UI.Base.Screen.Option as Screen
import Blessed.UI.Base.Screen.Event as Screen
import Blessed.UI.Base.Screen.Method as Screen
import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Boxes.Box.Event as Box
import Blessed.UI.Boxes.Box.Method as Box


mainScreen = nk :: Screen <^> "main-scr"
mainBox = nk :: Box <^> "main-box"


main :: Effect Unit
main = do
  Cli.run unit
    (B.screenAnd mainScreen

        [ Screen.title "foo"
        , Screen.smartCSR true

        , Screen.key
            [ Key.escape, Key.alpha 'q', (Key.control $ Key.alpha 'C') ]
            $ \screen kevt -> do
                Blessed.exit
        ]

        [ B.box mainBox
            [ Box.top Offset.center
            , Box.left Offset.center
            , Box.width $ Dimension.percents 50.0
            , Box.height $ Dimension.percents 50.0
            , Box.content "Hello {bold}world{/bold}!"
            , Box.tags true
            , Box.draggable true
            , Box.border [ Border.type_ Border._line, Border.fg "white" ]
            , Box.border [ Border.type_ Border._line, Border.fg "white" ]
            , Box.style
                [ Style.fg "white"
                , Style.bg "magenta"
                , Style.border
                    [ Border.fg "#f0f0f0"
                    ]
                , Style.hover
                    [ ES.bg "green"
                    ]
                ]

            , Box.on Box.click $ \box cevt -> do
                box # Box.setContent "{center}Some different {red-fg}content{/red-fg}.{/center}"
                mainScreen >~ Screen.render

            , Box.key (Key.only Key.enter) $ \box kevt -> do
                box # Box.setContent "{right}Even different {black-fg}content{/black-fg}.{/right}\n"
                box # Box.setLine 1 "bar"
                box # Box.insertLine 1 "foo"
                mainScreen >~ Screen.render
            ]

            []

        ]

        $ \screen -> do
            mainBox >~ Box.focus
            screen # Screen.render
    )
