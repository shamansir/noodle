module Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (Error)

import Node.HTTP (Request)

import Cli.App as Cli
import Cli.State (State)
import Cli.State (initial) as State
import Cli.WsServer as WSS

import Cli.Components.MainScreen as MainScreen

import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedOp as Blessed
import Web.Socket.Server as WSS


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
    Cli.runAnd State.initial MainScreen.component $ do
        hMsg <- Blessed.impair2 handleMessage
        hCon <- Blessed.impair2 handleConnection
        hErr <- Blessed.impair1 handleError
        hStart <- Blessed.impair1 handleStart
        wss <- liftEffect $ WSS.start
            { handleMessage : hMsg
            , handleConnection : hCon
            , handleError : hErr
            , handleStart : hStart
            }
        pure unit
    where
        handleStart :: Unit -> BlessedOp State Effect
        handleStart _ = pure unit
        handleMessage :: WSS.WebSocketConnection -> WSS.WebSocketMessage -> BlessedOp State Effect
        handleMessage _ _ = pure unit
        handleConnection :: WSS.WebSocketConnection -> Request -> BlessedOp State Effect
        handleConnection _ _ = pure unit
        handleError :: Error -> BlessedOp State Effect
        handleError _ = pure unit