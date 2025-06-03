module Front.Shared.WebSocketStatus where

import Prelude


data Status
    = Off
    | Waiting
    | Connected Int
