module WebSocket.Server.Server where
-- upgraded from : https://github.com/FruitieX/purescript-ws

import Prelude

import Effect (Effect)
import Effect.Uncurried (mkEffectFn1, mkEffectFn2, runEffectFn2, runEffectFn3)
import Effect.Exception (Error)
-- import Data.Proxy (Proxy)
import Type.Proxy (Proxy(..))
import Record (insert)
import Type.Row (class Lacks, class Cons, class Union) as Row
import Node.HTTP.Types (HttpServer, ClientRequest)
-- import Node.HTTP (HttpServer)

import WebSocket.Types
  ( WebSocketServer, Port(..), WebSocketServerOptions
  , WebSocketConnection
  )
import WebSocket.Server.Foreign
  ( createWebSocketServer_
  , onConnection_
  , onServerError_
  )


-- | Creates a WebSocket.Server and internally a HTTP server
-- | which binds to a given port
-- |
-- | The supplied callback is called when the created HTTP server
-- | starts listening.
createWebSocketServerWithPort
  :: forall options options' trash
   . Row.Union options options' WebSocketServerOptions
  => Row.Lacks "port" options
  => Row.Cons "port" Port options trash
  => Port
  -> { | options }
  -> (Unit -> Effect Unit)
  -> Effect WebSocketServer
createWebSocketServerWithPort (Port port) options callback =
  runEffectFn2 createWebSocketServer_ options' callback'
    where
      options' = insert (Proxy :: Proxy "port") port options
      callback' = mkEffectFn1 callback

-- | Creates a WebSocket.Server from a pre-existing Node.Server
createWebSocketServerWithServer
  :: forall options options' trash
   . Row.Union options options' WebSocketServerOptions
  => Row.Lacks "server" options
  => Row.Cons "server" HttpServer options trash
  => HttpServer
  -> { | options }
  -> Effect WebSocketServer
createWebSocketServerWithServer server options =
  runEffectFn2 createWebSocketServer_ options' callback'
    where
      options' = insert (Proxy :: Proxy "server") server options
      callback' = mkEffectFn1 $ const (pure unit)

-- | Attaches a connection event handler to a WebSocketServer
onConnection
  :: WebSocketServer
  -> (WebSocketConnection -> ClientRequest -> Effect Unit)
  -> Effect Unit
onConnection server callback =
  runEffectFn2 onConnection_ server (mkEffectFn2 callback)

-- | Attaches an error event handler to a WebSocketServer
onServerError
  :: WebSocketServer
  -> (Error -> Effect Unit)
  -> Effect Unit
onServerError server callback =
  runEffectFn2 onServerError_ server (mkEffectFn1 callback)