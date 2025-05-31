module Cli.WsServer where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.Traversable (for_)

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console as Console

-- import Web.Socket.Server as WSS


import Effect.Exception (Error)
import Node.HTTP.Types (ClientRequest)
import Web.Socket.Server as WSS

-- import Toolkit.Hydra.Lang (Program) as Lang
-- import Toolkit.Hydra.Lang.ToCode (toCode, javaScript) as Lang


type State = WSS.WebSocketServer /\ Array WSS.WebSocketConnection


start ::
  { handleStart :: Unit -> Effect Unit
  , handleConnection :: WSS.WebSocketConnection -> ClientRequest -> Effect Unit
  , handleError :: Error -> Effect Unit
  , handleMessage :: WSS.WebSocketConnection -> WSS.WebSocketMessage -> Effect Unit
  }
  -> Effect WSS.WebSocketServer
start def = do
  wss <- WSS.createWebSocketServerWithPort (WSS.Port 9999) {} def.handleStart
  WSS.onConnection wss $ \ws req -> do
    WSS.onMessage ws $ def.handleMessage ws
    WSS.onError ws def.handleError
    def.handleConnection ws req
  WSS.onServerError wss def.handleError
  pure wss


{-
broadcastProgram
  :: Lang.Program Unit -> State -> Effect Unit
broadcastProgram program (_ /\ connections) = do
  let programString = Lang.toCode Lang.javaScript program
  for_ connections $ \ws -> WSS.sendMessage ws $ WSS.WebSocketMessage programString
-}