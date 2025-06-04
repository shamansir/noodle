module Back.WSServer where

import Prelude

import Data.Map (Map)
import Data.Map (empty, insert, delete, size, toUnfoldable) as Map
import Data.Array (snoc, delete, filter) as Array
import Data.Tuple (fst, snd) as Tuple
import Data.UniqueHash (UniqueHash)
import Data.UniqueHash (generate) as UH
import Data.Traversable (for_)

import Effect (Effect)
import Effect.Console (log) as Console
import Effect.Ref (Ref)
import Effect.Ref (new, modify, modify_, read) as Ref


import WebSocket.Types (WebSocketServer, toMessage)
import WebSocket.Types (MinimumWebSocketServerOptions, WebSocketConnection, WebSocketMessage(..), Host(..), Port(..)) as WSS
import WebSocket.Server.Server as WSServer
import WebSocket.Server.Connection as WSConn

import Front.Shared.WsLocation (port) as WSLoc

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
    wsServer <- WSServer.createWebSocketServerWithPort WSLoc.port options $
        \_ -> Console.log $ "Noodle WS Server started at port "
                    <> show (case WSLoc.port of WSS.Port n -> show n)
                    <> ". Ctrl+C to stop server."
    wsServer # WSServer.handle (serverDef stateRef)


serverDef :: Ref ServerState -> Record WSServer.Def
serverDef stateRef =
    { onConnection : \conn req -> do
        connHash <- UH.generate
        Console.log $ "New connection: " <> show connHash
        WSConn.sendMessage conn $ toMessage $ WS.Waiting
        curState <- stateRef # Ref.modify \s -> s { connections = s.connections # Map.insert connHash conn }
        WSConn.sendMessage conn $ toMessage $ WS.CurrentConnection connHash
        let connectionsCount = Map.size curState.connections
        for_ (connectionsExcept connHash curState) $ \otherConn -> do
            WSConn.sendMessage otherConn $ toMessage $ WS.NewConnection connHash
            WSConn.sendMessage otherConn $ toMessage $ WS.ConnectionsCount connectionsCount
        WSConn.sendMessage conn $ toMessage $ WS.ConnectionsCount connectionsCount
        conn # WSConn.handle (connectionDef conn connHash stateRef)
    , onError : \err ->
        Console.log "Server error"
    }
    where
        connectionsExcept :: UniqueHash -> ServerState -> Array WSS.WebSocketConnection
        connectionsExcept exceptHash = _.connections >>> Map.toUnfoldable >>> Array.filter (Tuple.fst >>> (_ == exceptHash)) >>> map Tuple.snd


connectionDef :: WSS.WebSocketConnection -> UniqueHash -> Ref ServerState -> Record WSConn.Def
connectionDef conn connHash stateRef =
    { onOpen : \_ ->
        Console.log $ "Open connection (" <> show connHash <> ")"
    , onClose : \code reason -> do
        curState <- stateRef # Ref.modify \s -> s { connections = s.connections # Map.delete connHash }
        WSConn.sendMessage conn $ toMessage $ WS.ConnectionsCount $ Map.size curState.connections
        WSConn.sendMessage conn $ toMessage $ WS.Disconnected
        Console.log $ "Connection closed. (" <> show connHash <> "). " -- <> show code <> ". " <> show reason
    , onMessage : \(WSS.WebSocketMessage wsMsg) ->
        Console.log $ "Received message: " <> wsMsg <> ". (" <> show connHash <> ")"
    , onError : \err ->
        Console.log $"Error" <> ". (" <> show connHash <> ")"
    }


main :: Effect Unit
main = start