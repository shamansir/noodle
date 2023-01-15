module Main where

import Prelude


import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Console (log)

import Cli.App as Cli

import Blessed as B
import Blessed (exit) as Blessed
import Blessed.Core.Key as Key
import Blessed.Core.Offset as Offset
import Blessed.Core.Border as Border
import Blessed.Core.Dimension as Dimension
import Blessed.Core.Style as Style
import Blessed.UI.Screen as Screen
import Blessed.UI.Box as Box


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
                Box.setContent box "{center}Some different {red-fg}content{/red-fg}.{/center}"
                B.with_ "main-scr" $ \screen -> Screen.render screen

            , Box.on Box.click $ \box cevt -> do
                Box.setContent box "{right}Even different {black-fg}content{/black-fg}.{/right}\n"
                Box.setLine box 1 "bar"
                Box.insertLine box 1 "foo"
                B.with_ "main-scr" $ \screen -> Screen.render screen
            ]

            []

        ]

        $ \screen -> do
            B.with_ "main-box" $ \box -> Box.focus box
            Screen.render screen
    )
