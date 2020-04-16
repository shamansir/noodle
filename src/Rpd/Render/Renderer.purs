module Rpd.Render.Renderer where

import Prelude (($))
import Data.Covered
import Data.Tuple.Nested (type (/\))

import Rpd.API.Errors (RpdError)
import Rpd.API.Action (Action) as C
import Rpd.API.Action.Apply (apply) as C
import Rpd.Network (Network)
import Rpd.Toolkit (Toolkit)

import Rpd.Render.UI (CoveredUI)
import Rpd.Render.UI (makeWithPush) as UI


data Routed other core
    = FromCore core
    | FromUI other


type Renderer d c n action model view
    = CoveredUI RpdError (Routed action (C.Action d c n)) (model /\ Network d c n) view


type Minimal d c n view
    = CoveredUI RpdError (C.Action d c n) (Network d c n) view


makeMinimal
    :: forall d c n view
     . Toolkit d c n
    -> (Covered RpdError (Network d c n) -> view)
    -> Minimal d c n view
makeMinimal toolkit =
    UI.makeWithPush
        \pushAction action coveredNw -> C.apply toolkit pushAction action $ recover coveredNw
