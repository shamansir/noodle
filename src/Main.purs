module Main where

import Prelude


import Effect (Effect)

import Cli.App as Cli

import Blessed as B
import Blessed (exit) as Blessed
import Blessed.Core.Key as Key
import Blessed.Core.Offset as Offset
import Blessed.Core.Border as Border
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Style as Style


import Blessed.UI.Box as Box
import Blessed.UI.Screen as Screen


main :: Effect Unit
main = do
  Cli.run
    (B.screenAnd "main-scr"

        [ Screen.title "foo"
        , Screen.smartCSR true

        , Screen.key
            [ Key.escape, Key.alpha 'q', (Key.control $ Key.alpha 'C') ]
            $ \screen kevt -> do
                Blessed.exit
        ]

        [ B.box "main-box"
            [ Box.top Offset.center
            , Box.left Offset.center
            , Box.width $ Dimension.percents 50.0
            , Box.height $ Dimension.percents 50.0
            , Box.content "Hello {bold}world{/bold}!"
            , Box.tags true
            , Box.draggable true
            --, Box.border Border.line
            , Box.style
                [ Style.fg "white"
                , Style.bg "magenta"
                , Style.border
                    [ Border.fg "#f0f0f0"
                    ]
                , Style.hover
                    [ Style.bg "green"
                    ]
                ]

            , Box.key (Key.only Key.enter) $ \box kevt -> do
                box # Box.setContent "{center}Some different {red-fg}content{/red-fg}.{/center}"
                B.with_ (B.ref "main-scr") Screen.render

            , Box.on Box.click $ \box cevt -> do
                box # Box.setContent "{right}Even different {black-fg}content{/black-fg}.{/right}\n"
                box # Box.setLine 1 "bar"
                box # Box.insertLine 1 "foo"
                B.with_ (B.ref "main-scr") Screen.render
            ]

            []

        ]

        $ \screen -> do
            B.with_ (B.ref "main-box") Box.focus
            Screen.render screen
    )
