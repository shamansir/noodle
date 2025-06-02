module WebSocket.Types where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)


foreign import data WebSocket :: Type
foreign import data WebSocketServer :: Type
foreign import data WebSocketConnection :: Type -- FIXME: The same as `WebSocket`?


newtype WebSocketMessage = WebSocketMessage String
derive newtype instance showWSM :: Show WebSocketMessage
derive instance Newtype WebSocketMessage _


class IsWsMessage a where
  toMessage :: a -> WebSocketMessage
  fromMessage :: WebSocketMessage -> a


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
