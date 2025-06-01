module Web.Socket.Types where

import Prelude

import Data.Maybe (Maybe)


foreign import data WebSocketServer :: Type
foreign import data WebSocketConnection :: Type


newtype WebSocketMessage = WebSocketMessage String
derive newtype instance showWSM :: Show WebSocketMessage


-- TODO: more options from:
-- https://github.com/websockets/ws/blob/master/doc/ws.md
type WebSocketServerOptions =
  ( host :: Host
  , backlog :: Maybe Int
  , autoPong :: Boolean
  , allowSynchronousEvents :: Boolean
  , clientTracking :: Boolean
  , maxPayload :: Maybe Int
  , noServer :: Boolean
  , path :: Maybe String
  )


type MinimumWebSocketServerOptions =
  ( host :: Host
  )


newtype Host = Host String
newtype Port = Port Int
newtype Protocol = Protocol String

newtype CloseCode = CloseCode Int
newtype CloseReason = CloseReason String
