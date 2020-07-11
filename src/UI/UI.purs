module UI
    ( UI, CoveredUI
    , make, makeWithPush, run, once
    , view, update, update'
    , makeMinimal, makeWithNoEffects, makePassing
    , imapModel, imapAction, imapError
    , mapFSM
    ) where

import Prelude

import Effect (Effect)

import Data.List (List)
import Data.Tuple.Nested ((/\), type (/\))

import FRP.Event (Event)
import FRP.Event as Event

import Data.Covered (Covered, mapError)

import FSM (FSM)
import FSM as FSM
import Data.Foldable (class Foldable)

import Noodle.Util (Canceler)


data UI action model view = UI (FSM action model) (model -> view)


type CoveredUI error action model view = UI action (Covered error model) view


make
    :: forall action model view
     . (action -> model -> model /\ List (Effect action))
    -> (model -> view)
    -> UI action model view
make updateF viewF =
    UI (FSM.make updateF) viewF


makeWithPush
    :: forall action model view
     . ((action -> Effect Unit) -> action -> model -> model /\ List (Effect action))
    -> (model -> view)
    -> UI action model view
makeWithPush updateF viewF =
    UI (FSM.makeWithPush updateF) viewF


makeMinimal
    :: forall action model view
     . (action -> model -> model)
    -> (model -> view)
    -> UI action model view
makeMinimal updateF viewF =
    UI (FSM.makeMinimal updateF) viewF


makeWithNoEffects
    :: forall action model view
     . (action -> model -> model /\ List action)
    -> (model -> view)
    -> UI action model view
makeWithNoEffects updateF viewF =
    UI (FSM.makeWithNoEffects updateF) viewF


makePassing
    :: forall action model view
     . (model -> view)
    -> UI action model view
makePassing viewF =
    UI FSM.makePassing viewF


run
    :: forall action model view
     . UI action model view
    -> model
    -> Effect
        { next :: Event view
        , push :: action -> Effect Unit
        , stop :: Canceler
        }
run (UI fsm viewF) model = do
    { event : views, push : pushView } <- Event.create
    { push, stop } <- FSM.run' fsm model (pushView <<< viewF)
    pure
        { next : views
        , push : push
        , stop
        }


view :: forall action model view. UI action model view -> model -> view
view (UI _ viewF) = viewF


update
    :: forall action model view
     . UI action model view
    -> (action -> Effect Unit)
    -> action
    -> model
    -> model /\ List (Effect action)
update (UI fsm _) = FSM.update fsm


update'
    :: forall action model view
     . UI action model view
    -> action
    -> model
    -> model /\ List (Effect action)
update' (UI fsm _) = FSM.update' fsm


-- TODO: get rid of this function by making a `RollbackFSM` to be a `newtype`
--       or just by always considering UI to be "Covered"
mapFSM
    :: forall action model view
     . (FSM action model -> FSM action model)
    -> UI action model view
    -> UI action model view
mapFSM f (UI fsm viewF)
    = UI (f fsm) viewF


imapModel
    :: forall action modelA modelB view
     . (modelA -> modelB)
    -> (modelB -> modelA)
    -> UI action modelA view
    -> UI action modelB view
imapModel mapAToB mapBToA (UI fsm viewF) =
    UI
        (fsm # FSM.imapModel mapAToB mapBToA)
        (viewF <<< mapBToA)


imapAction
    :: forall actionA actionB model view
     . (actionA -> actionB)
    -> (actionB -> actionA)
    -> UI actionA model view
    -> UI actionB model view
imapAction mapAToB mapBToA (UI fsm viewF) =
    UI
        (fsm # FSM.imapAction mapAToB mapBToA)
        viewF


imapError
    :: forall errorA errorB action model view
     . (errorA -> errorB)
    -> (errorB -> errorA)
    -> CoveredUI errorA action model view
    -> CoveredUI errorB action model view
imapError mapAToB mapBToA =
    imapModel (mapError mapAToB) (mapError mapBToA)


once
    :: forall action model view
     . UI action model view
    -> model
    -> view
once (UI _ viewF) model = viewF model


{- render
    :: forall action model view
     . UI action model view
    -> model
    -> view
render = once -}
