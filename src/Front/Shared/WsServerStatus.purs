module Front.Shared.WsServerStatus where

import Prelude


data Status
    = Off
    | Waiting
    | Connected Int
