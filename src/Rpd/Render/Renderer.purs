module Rpd.Render.Renderer where

import Prelude (($))
import Data.Covered
import Data.Tuple.Nested (type (/\))

import FSM (class DoNothing, doNothing)

import Rpd.API.Errors (RpdError)
import Rpd.API.Action as C
import Rpd.API.Action.Apply as C
import Rpd.Network (Network)
import Rpd.Toolkit (Toolkit)

import Rpd.Render.UI (CoveredUI)
import Rpd.Render.UI (make) as UI


data Routed other core
    = FromCore core
    | FromUI other


type Renderer d c n action model view
    = CoveredUI RpdError (Routed action (C.Action d c n)) (model /\ Network d c n) view


instance routedDoNothing :: DoNothing (Routed action (C.Action d c n)) where
    doNothing = FromCore C.NoOp


type Minimal d c n view
    = CoveredUI RpdError (C.Action d c n) (Network d c n) view


makeMinimal
    :: forall d c n view
     . Toolkit d c n
    -> (Covered RpdError (Network d c n) -> view)
    -> Minimal d c n view
makeMinimal toolkit =
    UI.make
        \action coveredNw -> C.apply toolkit action $ recover coveredNw
