module WebSocket.Server.Connection where

import Prelude

import Effect (Effect)
import Effect.Uncurried (mkEffectFn1, mkEffectFn2, runEffectFn2, runEffectFn3)
import Effect.Exception (Error)

import WebSocket.Types
  ( WebSocketMessage, CloseCode(..), CloseReason(..)
  , WebSocketConnection
  )
import WebSocket.Server.Foreign
  ( onMessage_
  , onOpen_
  , onClose_
  , onError_
  , sendMessage_
  , close_
  )


-- | Attaches a open event handler to a WebSocketConnection
onOpen
  :: WebSocketConnection
  -> (Unit -> Effect Unit)
  -> Effect Unit
onOpen ws callback =
  runEffectFn2 onOpen_ ws (mkEffectFn1 callback)

-- | Attaches a close event handler to a WebSocketConnection
onClose
  :: WebSocketConnection
  -> (CloseCode -> CloseReason -> Effect Unit)
  -> Effect Unit
onClose ws callback =
  runEffectFn2 onClose_ ws (mkEffectFn2 callback)

-- | Attaches a message event handler to a WebSocketConnection
onMessage
  :: WebSocketConnection
  -> (WebSocketMessage -> Effect Unit)
  -> Effect Unit
onMessage ws callback =
  runEffectFn2 onMessage_ ws (mkEffectFn1 callback)

-- | Attaches an error event handler to a WebSocketConnection
onError
  :: WebSocketConnection
  -> (Error -> Effect Unit)
  -> Effect Unit
onError ws callback =
  runEffectFn2 onError_ ws (mkEffectFn1 callback)

-- | Send a message over a WebSocketConnection
sendMessage
  :: WebSocketConnection
  -> WebSocketMessage
  -> Effect Unit
sendMessage ws message =
  runEffectFn2 sendMessage_ ws message

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


type Def =
  ( onOpen :: Unit -> Effect Unit
  , onClose :: CloseCode -> CloseReason -> Effect Unit
  , onMessage :: WebSocketMessage -> Effect Unit
  , onError :: Error -> Effect Unit
  )

handle :: Record Def -> WebSocketConnection -> Effect Unit
handle def ws = do
  onOpen    ws def.onOpen
  onClose   ws def.onClose
  onMessage ws def.onMessage
  onError   ws def.onError

doNothing =
  { onOpen : const $ pure unit
  , onClose : const $ const $ pure unit
  , onMessage : const $ pure unit
  , onError : const $ pure unit
  } :: Record Def