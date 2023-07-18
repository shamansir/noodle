module Cli.WsClient where

import Prelude

import Effect (Effect)

import Web.Socket.WebSocket as WS


main :: Effect Unit
main = do
    {-
    ws <- WS.create "ws://localhost:5555" []
    -- let et = WS.toEventTarget ws
    WS.sendString ws "TEST"
    -}
    pure unit