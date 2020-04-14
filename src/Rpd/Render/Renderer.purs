module Rpd.Render.Renderer where

import Data.Either
import Data.Tuple.Nested (type (/\))

import FSM (class DoNothing, doNothing)

import Rpd.API.Errors
import Rpd.API.Action as C
import Rpd.Network

import Rpd.Render.UI


data Routed other core
    = FromCore core
    | FromUI other


type Renderer d c n action model view
    = CoveredUI RpdError (Routed action (C.Action d c n)) (model /\ Network d c n) view


instance routedDoNothing :: DoNothing (Routed action (C.Action d c n)) where
    doNothing = FromCore C.NoOp


type Minimal d c n view
    = UI (C.Action d c n) (Network d c n) view
