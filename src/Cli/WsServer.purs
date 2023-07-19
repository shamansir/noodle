module Cli.WsServer where
-- example upgraded from : https://github.com/FruitieX/purescript-ws

import Prelude

import Effect (Effect)

-- import Web.Socket.Server as WSS


import Effect.Console (log) as Console
import Effect.Exception (Error)
import Node.HTTP (Request)
import Web.Socket.Server (Port(Port), WebSocketConnection, WebSocketMessage(WebSocketMessage), close, createWebSocketServerWithPort, onConnection, onError, onMessage, onServerError, sendMessage) as WSS

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

main
  :: Effect Unit
main = do
  wss <- WSS.createWebSocketServerWithPort (WSS.Port 9999) {} $ const do
    Console.log "Listening on port 9999."
  WSS.onConnection wss handleConnection
  WSS.onServerError wss handleError