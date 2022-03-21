module Noodle.Fn.Protocol where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Map.Extra (type (/->))
import Data.Tuple.Nested (type (/\))


type Protocol i o d = -- a.k.a. Transport
    { receive :: i -> Effect (Maybe d)
    , send :: o -> d -> Effect Unit
    , sendIn :: i -> d -> Effect Unit
    , last :: Maybe i
    }