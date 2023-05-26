module Main where

import Prelude

import Effect (Effect)

import Cli.App as Cli
import Cli.State (initial) as State

import Cli.Components.MainScreen as MainScreen



{-
testPalette :: Effect Unit
testPalette =
  Cli.run unit
    (B.screenAnd Key.mainScreen

        [ Screen.title "Palette"
        , Screen.smartCSR true
        , Screen.fullUnicode true
        , Screen.key
            [ Key.escape, Key.alpha 'q', (Key.control $ Key.alpha 'C') ]
            $ \_ kevt -> do
                Blessed.exit
        ]

        [ PaletteList.component 0 0 0.0 0.0
        ]

        $ \_ -> do
            Key.mainScreen >~ Screen.render
    )
-}

main :: Effect Unit
main =
    Cli.run State.initial MainScreen.component