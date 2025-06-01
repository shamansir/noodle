module Web.Socket.Foreign where
-- upgraded from : https://github.com/FruitieX/purescript-ws

import Prelude

import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3)
import Effect.Exception (Error)
-- import Data.Proxy (Proxy)
import Node.HTTP.Types (ClientRequest)
-- import Node.HTTP (HttpServer)
import Web.Socket.Types
  ( WebSocketServer, WebSocketConnection, WebSocketMessage
  , CloseCode, CloseReason, Host, Port, Protocol
  )


foreign import createWebSocketServer_
  :: forall options
   . EffectFn2 options
     (EffectFn1 Unit Unit)
     WebSocketServer

foreign import createWebSocket_
  :: EffectFn3
     Host
     Port
     (Array Protocol)
     WebSocketConnection

foreign import onConnection_
  :: EffectFn2
     WebSocketServer
     (EffectFn2 WebSocketConnection ClientRequest Unit)
     Unit

foreign import onServerError_
  :: EffectFn2
     WebSocketServer
     (EffectFn1 Error Unit)
     Unit

foreign import onMessage_
  :: EffectFn2
     WebSocketConnection
     (EffectFn1 WebSocketMessage Unit)
     Unit

foreign import onOpen_
  :: EffectFn2
     WebSocketConnection
     (EffectFn1 Unit Unit)
     Unit

foreign import onClose_
  :: EffectFn2
     WebSocketConnection
     (EffectFn2 CloseCode CloseReason Unit)
     Unit

foreign import onError_
  :: EffectFn2
     WebSocketConnection
     (EffectFn1 Error Unit)
     Unit

foreign import sendMessage_
  :: EffectFn2
     WebSocketConnection
     WebSocketMessage
     Unit

foreign import close_
  :: EffectFn3
     WebSocketConnection
     CloseCode
     CloseReason
     Unit