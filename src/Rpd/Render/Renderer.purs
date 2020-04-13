module Rpd.Render.Renderer where

import Data.Either
import Data.Tuple.Nested (type (/\))


import Rpd.API.Errors
import Rpd.API.Action as C
import Rpd.Network

import Rpd.Render.UI


type Renderer d c n action model view
    = CoveredUI RpdError (Either action (C.Action d c n)) (model /\ Network d c n) view


type Minimal d c n view
    = UI (C.Action d c n) (Network d c n) view
