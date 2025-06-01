module WebSocket.Client.Foreign where
-- upgraded from : https://github.com/FruitieX/purescript-ws

import Prelude

import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3)
import Effect.Exception (Error)
import WebSocket.Types
  ( WebSocket, WebSocketMessage
  , CloseCode, CloseReason, Host, Port, Protocol
  )


foreign import createWebSocket_
  :: EffectFn3
     Host
     Port
     (Array Protocol)
     WebSocket

foreign import onMessage_
  :: EffectFn2
     WebSocket
     (EffectFn1 WebSocketMessage Unit)
     Unit

foreign import onOpen_
  :: EffectFn2
     WebSocket
     (EffectFn1 Unit Unit)
     Unit

foreign import onClose_
  :: EffectFn2
     WebSocket
     (EffectFn2 CloseCode CloseReason Unit)
     Unit

foreign import onError_
  :: EffectFn2
     WebSocket
     (EffectFn1 Error Unit)
     Unit

foreign import sendMessage_
  :: EffectFn2
     WebSocket
     WebSocketMessage
     Unit

foreign import close_
  :: EffectFn3
     WebSocket
     CloseCode
     CloseReason
     Unit