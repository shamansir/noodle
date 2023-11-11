module Prev.Web.Socket.Server where
-- upgraded from : https://github.com/FruitieX/purescript-ws

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, runEffectFn2, runEffectFn3)
import Effect.Exception (Error)
-- import Data.Proxy (Proxy)
import Type.Proxy (Proxy(..))

import Data.Newtype (class Newtype)
import Record (insert)
import Type.Row (class Lacks, class Cons, class Union) as Row
import Node.HTTP (Request, Server)


foreign import data WebSocketServer :: Type
foreign import data WebSocketConnection :: Type


newtype WebSocketMessage = WebSocketMessage String
derive newtype instance showWSM :: Show WebSocketMessage
derive instance newtypeWSM :: Newtype WebSocketMessage _



-- TODO: more options from:
-- https://github.com/websockets/ws/blob/master/doc/ws.md
type WebSocketServerOptions =
  ( host :: String
  , backlog :: Int
  )


newtype Port = Port Int

newtype CloseCode = CloseCode Int
newtype CloseReason = CloseReason String


foreign import createWebSocketServer_
  :: forall options
   . EffectFn2 options
     (EffectFn1 Unit Unit)
     WebSocketServer

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
  => Row.Cons "server" Server options trash
  => Server
  -> { | options }
  -> Effect WebSocketServer
createWebSocketServerWithServer server options =
  runEffectFn2 createWebSocketServer_ options' callback'
    where
      options' = insert (Proxy :: Proxy "server") server options
      callback' = mkEffectFn1 $ const (pure unit)

foreign import onConnection_
  :: EffectFn2
     WebSocketServer
     (EffectFn2 WebSocketConnection Request Unit)
     Unit

-- | Attaches a connection event handler to a WebSocketServer
onConnection
  :: WebSocketServer
  -> (WebSocketConnection -> Request -> Effect Unit)
  -> Effect Unit
onConnection server callback =
  runEffectFn2 onConnection_ server (mkEffectFn2 callback)

foreign import onServerError_
  :: EffectFn2
     WebSocketServer
     (EffectFn1 Error Unit)
     Unit

-- | Attaches an error event handler to a WebSocketServer
onServerError
  :: WebSocketServer
  -> (Error -> Effect Unit)
  -> Effect Unit
onServerError server callback =
  runEffectFn2 onServerError_ server (mkEffectFn1 callback)

foreign import onMessage_
  :: EffectFn2
     WebSocketConnection
     (EffectFn1 WebSocketMessage Unit)
     Unit

-- | Attaches a message event handler to a WebSocketConnection
onMessage
  :: WebSocketConnection
  -> (WebSocketMessage -> Effect Unit)
  -> Effect Unit
onMessage ws callback =
  runEffectFn2 onMessage_ ws (mkEffectFn1 callback)

foreign import onClose_
  :: EffectFn2
     WebSocketConnection
     (EffectFn2 CloseCode CloseReason Unit)
     Unit

-- | Attaches a close event handler to a WebSocketConnection
onClose
  :: WebSocketConnection
  -> (CloseCode -> CloseReason -> Effect Unit)
  -> Effect Unit
onClose ws callback =
  runEffectFn2 onClose_ ws (mkEffectFn2 callback)

foreign import onError_
  :: EffectFn2
     WebSocketConnection
     (EffectFn1 Error Unit)
     Unit

-- | Attaches an error event handler to a WebSocketConnection
onError
  :: WebSocketConnection
  -> (Error -> Effect Unit)
  -> Effect Unit
onError ws callback =
  runEffectFn2 onError_ ws (mkEffectFn1 callback)

foreign import sendMessage_
  :: EffectFn2
     WebSocketConnection
     WebSocketMessage
     Unit

-- | Send a message over a WebSocketConnection
sendMessage
  :: WebSocketConnection
  -> WebSocketMessage
  -> Effect Unit
sendMessage ws message =
  runEffectFn2 sendMessage_ ws message

foreign import close_
  :: EffectFn3
     WebSocketConnection
     CloseCode
     CloseReason
     Unit

-- | Initiate a closing handshake
close
  :: WebSocketConnection
  -> Effect Unit
close ws =
  -- 1000 is the CloseCode for normal closure
  runEffectFn3 close_ ws (CloseCode 1000) (CloseReason "Closed by server")

-- | Initiate a closing handshake with given code and reason
close'
  :: WebSocketConnection
  -> CloseCode
  -> CloseReason
  -> Effect Unit
close' ws code reason =
  runEffectFn3 close_ ws code reason