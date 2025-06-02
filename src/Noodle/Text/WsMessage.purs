module Noodle.Text.WsMessage where

import Prelude


import Data.UniqueHash (UniqueHash)


data Message
    = NewConnection UniqueHash
    | ConnectionsCount Int
    | CurrentConnection UniqueHash
    | NdfCommand Unit
    | HydraScene Unit -- FIXME: change to some generic toolkit message, e.g. serialized patch state


-- (\{- REM)|(-- REM)