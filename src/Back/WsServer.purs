module Back.WsServer where

import Prelude

import Data.Map (Map)
import Data.Map (empty, insert, delete) as Map
import Data.Array (snoc, delete) as Array

import Effect (Effect)
import Effect.Console (log) as Console
import Effect.Ref (new, modify, modify_) as Ref

import Data.UniqueHash (UniqueHash)
import Data.UniqueHash (generate) as UH

import Web.Socket.Server (WebSocketServer)
import Web.Socket.Server as WSS


options =
    { host : "localhost" -- TODO : take host (and port) from parameters
    }
    :: Record WSS.MinimumWebSocketServerOptions


type State =
    { connections :: Map UniqueHash WSS.WebSocketConnection
    }


init :: State
init =
    { connections : Map.empty
    }


main :: Effect Unit
main = do
    state <- Ref.new init
    wsServer <- WSS.createWebSocketServerWithPort (WSS.Port 3555) options $
        \_ -> Console.log $ "Noodle WS Server started at port " <> show 3555 <> ". Ctrl+C to stop server."
    WSS.onConnection wsServer $
        \conn req -> do
            connHash <- UH.generate
            Console.log $ "New connection: " <> show connHash
            state # Ref.modify_ \s -> s { connections = s.connections # Map.insert connHash conn }
            WSS.onMessage conn $
                \(WSS.WebSocketMessage wsMsg) -> do
                    Console.log $ "Received message: " <> wsMsg
            WSS.onClose conn $
                \code reason -> do
                    state # Ref.modify_ \s -> s { connections = s.connections # Map.delete connHash }
                    Console.log $ "Connection closed." -- "connection closed: " <> show code <> ". " <> show reason
            WSS.onError conn $
                \err ->
                    Console.log "Error"
            WSS.sendMessage conn $ WSS.WebSocketMessage "test"
            WSS.close conn
    WSS.onServerError wsServer $
        \err -> Console.log "Server error"