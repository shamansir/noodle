module Web.Socket.Client where


import Effect (Effect)
import Effect.Uncurried (runEffectFn3)


import Web.Socket.Types
  ( Host, Port, Protocol, WebSocketConnection )
import Web.Socket.Foreign
  ( createWebSocket_
  )

-- | Creates a WebSocket as the client.
createWebSocket
  :: Host
  -> Port
  -> Array Protocol
  -> Effect WebSocketConnection
createWebSocket host port protocols =
  runEffectFn3 createWebSocket_ host port protocols