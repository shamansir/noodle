module Back.WsServer where

import Prelude

import Data.Map (Map)
import Data.Map (empty, insert, delete, size) as Map
import Data.Array (snoc, delete) as Array

import Effect (Effect)
import Effect.Console (log) as Console
import Effect.Ref (Ref)
import Effect.Ref (new, modify, modify_, read) as Ref

import Data.UniqueHash (UniqueHash)
import Data.UniqueHash (generate) as UH

import WebSocket.Types (WebSocketServer, toMessage)
import WebSocket.Types (MinimumWebSocketServerOptions, WebSocketConnection, WebSocketMessage(..), Host(..), Port(..)) as WSS
import WebSocket.Server.Server as WSServer
import WebSocket.Server.Connection as WSConn

import Noodle.Text.WsMessage (Message(..)) as WS


options =
    { host : WSS.Host "localhost" -- TODO : take host (and port) from program parameters
    }
    :: Record WSS.MinimumWebSocketServerOptions


type ServerState =
    { connections :: Map UniqueHash WSS.WebSocketConnection
    }


{-
type ServerDef =
    { handleStart :: Unit -> Effect Unit
    , handleConnection :: WSS.WebSocketConnection -> ClientRequest -> Effect Unit
    , handleError :: Error -> Effect Unit
    , handleMessage :: WSS.WebSocketConnection -> WSS.WebSocketMessage -> Effect Unit
    }
-}

init :: ServerState
init =
    { connections : Map.empty
    }


start :: Effect Unit
start = do
    stateRef <- Ref.new init
    wsServer <- WSServer.createWebSocketServerWithPort (WSS.Port 3555) options $
        \_ -> Console.log $ "Noodle WS Server started at port " <> show 3555 <> ". Ctrl+C to stop server."
    wsServer # WSServer.handle (serverDef stateRef)


serverDef :: Ref ServerState -> Record WSServer.Def
serverDef stateRef =
    { onConnection : \conn req -> do
        connHash <- UH.generate
        Console.log $ "New connection: " <> show connHash
        WSConn.sendMessage conn $ toMessage $ WS.Waiting
        curState <- stateRef # Ref.modify \s -> s { connections = s.connections # Map.insert connHash conn }
        WSConn.sendMessage conn $ toMessage $ WS.NewConnection connHash
        WSConn.sendMessage conn $ toMessage $ WS.ConnectionsCount $ Map.size curState.connections
        conn # WSConn.handle (connectionDef conn connHash stateRef)
    , onError : \err ->
        Console.log "Server error"
    }


connectionDef :: WSS.WebSocketConnection -> UniqueHash -> Ref ServerState -> Record WSConn.Def
connectionDef conn connHash stateRef =
    { onOpen : \_ ->
        Console.log "Open connection"
    , onClose : \code reason -> do
        curState <- stateRef # Ref.modify \s -> s { connections = s.connections # Map.delete connHash }
        WSConn.sendMessage conn $ toMessage $ WS.ConnectionsCount $ Map.size curState.connections
        WSConn.sendMessage conn $ toMessage $ WS.Disconnected
        Console.log $ "Connection closed." -- "connection closed: " <> show code <> ". " <> show reason
    , onMessage : \(WSS.WebSocketMessage wsMsg) ->
        Console.log $ "Received message: " <> wsMsg
    , onError : \err ->
        Console.log "Error"
    }


main :: Effect Unit
main = start