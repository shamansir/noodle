module Rpd.Render.UI
    ( UI(..)
    , PushF(..)
    -- , make
    -- , once
    , Perform
    ) where



import Prelude

import Effect (Effect)

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))

import FRP.Event (Event)
import FRP.Event as Event

import FSM.Covered (Covered, recover, inject, hasError, cover')

import Rpd.Network (Network) as R
-- import Rpd.API as R
import Rpd.API.Action (Action) as Core
import Rpd.API.Action.Apply (apply, performEffect) as Core
import Rpd.API.Action.Sequence (prepare_) as ActionSeq
import Rpd.API.Errors (RpdError) as R
import Rpd.Toolkit as T
import Rpd.Render.Minimal as Minimal
import Rpd.Util (Canceler)


data PushF action  =
    PushF (action -> Effect Unit)


data Perform action =
    Perform (Unit -> Effect (Array action))


data UI model action error view
    = UI
        { init :: model
        , update :: action -> Covered error model -> model /\ Perform action
        , view :: model -> view
        }


noEffects :: forall action. Perform action
noEffects = Perform $ const $ pure []


neverPush :: forall action. PushF action
neverPush = PushF $ const $ pure unit


{-
make
    :: forall model view action
     . UI model view action
    -> Effect
        { first :: view
        , next :: Event view
        , push :: PushF action
        , stop :: Canceler
        }
make (UI { from, init, update, view, performEffect }) toolkit initialNW = do
    { models, pushAction, stop } <-
        ActionSeq.prepare_
            (init initialNW /\ initialNW)
            myApply
            myPerformEff
    { event : views, push : pushView } <- Event.create
    stopViews <- Event.subscribe models $ recover >>> view >>> pushView
    pure
        { first : from
        , next : views
        , push : PushF pushAction
        , stop : stop <> stopViews
        }
    where
        myApply (Right coreAction) covered = do
            let model /\ nw = recover covered
            nw' /\ coreEffects <- Core.apply toolkit coreAction nw
            let model' /\ userEffects =
                    update
                        toolkit
                        (Right coreAction)
                        (inject (model /\ nw') covered)
            let allEffects = (Right <$> coreEffects) <> (Left <$> userEffects)
            pure $ (model' /\ nw') /\ allEffects
        myApply (Left userAction) covered = do
            let
                _ /\ nw = recover covered
                model' /\ userEffects =
                    update
                        toolkit
                        (Left userAction)
                        covered
            pure $ (model' /\ nw) /\ (Left <$> userEffects)
        myPerformEff pushAction (Right coreEffect) (_ /\ nw) =
            Core.performEffect toolkit (pushAction <<< Right) coreEffect nw
        myPerformEff pushAction (Left userEffect) (model /\ nw) =
            performEffect toolkit pushAction userEffect (model /\ nw)


once
    :: forall model view action effect
     . UI model view action effect
    -> view
once (UI { init, view }) _ nw =
    view $ init nw /\ nw

-}
