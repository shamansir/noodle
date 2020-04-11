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



data UI action model view = UI view (model -> view) (FSM action model)
-- TODO: make it CoveredFSM


make
    :: forall action model view
     . (action -> model -> model /\ Effect action)
    -> view
    -> UI model action view
make updateF view =
    FSM.make


run
    :: forall action model view f
     . Monoid action
    => Foldable f
    => UI action model view
    -> model
    -> f action
    -> Effect
        { first :: view
        , next :: Event view
        , push :: action -> Effect Unit
        , stop :: Canceler
        }
run (UI firstView view fsm) model actions = do
    { event : views, push : pushView } <- Event.create
    { pushAction, stop } <- FSM.run' fsm model (pushView <<< view) actions
    pure
        { first : firstView
        , next : views
        , push : pushAction
        , stop
        }
