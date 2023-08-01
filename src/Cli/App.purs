module Cli.App where

import Prelude


import Data.Maybe (fromMaybe)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (Error)

import Control.Monad.State (modify_, get) as State

import Node.HTTP (Request)

import Cli.State (State)
import Cli.State (initial, registerWsClient, connectionsCount, informWsListening, informWsInitialized) as State
import Cli.WsServer as WSS
import Cli.Keys (mainScreen, wsStatusButton)

import Cli.Components.MainScreen as MainScreen
import Cli.Components.WsStatusButton as WsButton

import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedOp as Blessed
import Blessed.Internal.Core (Blessed)
import Blessed ((>~))
import Blessed (run, runAnd) as Blessed
import Blessed.UI.Base.Screen.Method as Screen

import Web.Socket.Server as WSS



run :: Effect Unit
run = Blessed.runAnd State.initial MainScreen.component $ do
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
        State.modify_ $ State.informWsInitialized wss
        mainScreen >~ Screen.render
        pure unit
    where
        handleStart :: Unit -> BlessedOp State Effect
        handleStart _ =  do
            State.modify_ State.informWsListening
            WsButton.updateStatus $ WsButton.Waiting
            mainScreen >~ Screen.render
        handleMessage :: WSS.WebSocketConnection -> WSS.WebSocketMessage -> BlessedOp State Effect
        handleMessage _ _ = pure unit
        handleConnection :: WSS.WebSocketConnection -> Request -> BlessedOp State Effect
        handleConnection wss _ = do
            State.modify_ $ State.registerWsClient wss
            state <- State.get
            WsButton.updateStatus $ WsButton.Connected $ fromMaybe 0 $ State.connectionsCount state
            mainScreen >~ Screen.render
            liftEffect $ WSS.sendMessage wss $ WSS.WebSocketMessage "ACK"
        handleError :: Error -> BlessedOp State Effect
        handleError _ = pure unit