module Main where

import Prelude


import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Console (log)

import Cli.App as Cli

import Blessed as B
import Blessed.UI.Node (Node)
import Blessed.UI.Screen as Screen


main :: Effect Unit
main = do
  Cli.run
    $ B.screenAnd "main-scr"

        [ Screen.title "foo"
        , Screen.smartCSR true

        , Screen.key
            [ Key.escape, Key.alpha "q", (Key.control $ Key.alpha "C") ]
            $ \screen ch key -> do
                Blessed.exit
        ]

        [ B.box "main-box"
            [ Box.top Offset.center
            , Box.left Offset.center
            , Box.width $ Dimension.percents 50
            , Box.height $ Dimension.percents 50
            , Box.content "Hello {bold}world{/bold}!"
            , Box.tags true
            , Box.draggable true
            , Box.border Box.line
            , Box.style
                [ Style.fg "white"
                , Style.bg "magenta"
                , Box.border
                    [ Box.fg "#f0f0f0"
                    ]
                , Box.hover $ const [ Style.bg green ]
                ]

            , Box.key (Key.one Key.enter) $ \box ch key -> do
                Box.setContent box "{center}Some different {red-fg}content{/red-fg}.{/center}"
                with_ "main-scr" $ \screen -> Screen.render screen

            , Box.on Box.click $ \box -> do
                Box.setContent box "{right}Even different {black-fg}content{/black-fg}.{/right}\n"
                Box.setLine 1 "bar"
                Box.insertLine 1 "foo"
                with_ "main-scr" $ \screen -> Screen.render screen
            ]

        ]
    $ \screen -> do
        with_ "main-box" $ \box -> Box.focus
        Screen.render screen
