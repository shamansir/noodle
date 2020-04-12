module Rpd.Render.UI
    ( UI
    , make, run
    ) where



import Prelude

import Effect (Effect)

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))

import FRP.Event (Event)
import FRP.Event as Event

import Data.Covered (Covered, recover, inject, hasError, cover')

import FSM (FSM)
import FSM (run, run', make) as FSM
import Data.Foldable (class Foldable)


import Rpd.Network (Network) as R
-- import Rpd.API as R
import Rpd.API.Action (Action) as Core
import Rpd.API.Action.Apply (apply) as Core
import Rpd.API.Action.Sequence (make) as ActionSeq
import Rpd.API.Errors (RpdError) as R
import Rpd.Toolkit as T
import Rpd.Render.Minimal as Minimal
import Rpd.Util (Canceler)



data UI action model view = UI (FSM action model) (model -> view)
-- TODO: make it CoveredFSM


make
    :: forall action model view
     . (action -> model -> model /\ Effect action)
    -> (model -> view)
    -> UI action model view
make updateF viewF =
    UI (FSM.make updateF) viewF


run
    :: forall action model view f
     . Monoid action
    => Foldable f
    => UI action model view
    -> view
    -> model
    -> f action
    -> Effect
        { next :: Event view
        , push :: action -> Effect Unit
        , stop :: Canceler
        }
run (UI fsm viewF) view model actions = do
    { event : views, push : pushView } <- Event.create
    { pushAction, stop } <- FSM.run' fsm model (pushView <<< viewF) actions
    pure
        { next : views
        , push : pushAction
        , stop
        }
