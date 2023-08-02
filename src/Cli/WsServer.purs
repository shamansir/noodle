module Cli.WsServer where
-- example upgraded from : https://github.com/FruitieX/purescript-ws

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.Traversable (for_)

import Effect (Effect)
import Effect.Class (class MonadEffect)

-- import Web.Socket.Server as WSS


import Effect.Exception (Error)
import Node.HTTP (Request)
import Web.Socket.Server as WSS

import Toolkit.Hydra2.Lang (formProgram, Program) as Lang
import Toolkit.Hydra2.Lang.ToCode (toCode, javaScript) as Lang


{-
handleMessage :: WSS.WebSocketConnection -> WSS.WebSocketMessage -> Effect Unit
handleMessage ws (WSS.WebSocketMessage msg) = do
  Console.log $ "received " <> msg
  WSS.close ws


handleError :: Error -> Effect Unit
handleError err = do
  Console.log $ show err


handleConnection :: WSS.WebSocketConnection -> Request -> Effect Unit
handleConnection ws req = do
  Console.log "Connected!"
  WSS.onMessage ws $ handleMessage ws
  WSS.onError ws handleError
  WSS.sendMessage ws $ WSS.WebSocketMessage "Hello, world!"
-}

type State = WSS.WebSocketServer /\ Array WSS.WebSocketConnection


start ::
  { handleStart :: Unit -> Effect Unit
  , handleConnection :: WSS.WebSocketConnection -> Request -> Effect Unit
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


broadcastProgram
  :: Lang.Program Unit -> State -> Effect Unit
broadcastProgram program (_ /\ connections) = do
  let programString = Lang.toCode Lang.javaScript program
  for_ connections $ \ws -> WSS.sendMessage ws $ WSS.WebSocketMessage programString
    -- WSS.sendMessage conn $ WSS.WebSocketMessage programString
  -- WSS.sendMessage conn $ WSS.WebSocketMessage programString


{-
main
  :: Effect Unit
main = do
  wss <- WSS.createWebSocketServerWithPort (WSS.Port 9999) {} $ const do
    Console.log "Listening on port 9999."
  WSS.onConnection wss handleConnection
  WSS.onServerError wss handleError
-}