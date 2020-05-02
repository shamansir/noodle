module Noodle.Render.Renderer where

import Prelude
import Effect (Effect)

import FSM (AndThen)
import FSM (joinWith) as FSM

import Data.Covered
import Data.Tuple.Nested ((/\), type (/\))

import Noodle.API.Errors (NoodleError)
import Noodle.API.Action (Action) as C
import Noodle.API.Action.Apply (apply) as C
import Noodle.Network (Network)
import Noodle.Toolkit (Toolkit)

import UI (CoveredUI)
import UI (makeWithPush, mapFSM) as UI


data Routed other core
    = FromCore core
    | FromUI other


type Renderer d c n action model view
    = CoveredUI NoodleError (Routed action (C.Action d c n)) (model /\ Network d c n) view


type UpdateF d c n action model view
    = (Routed action (C.Action d c n) -> Effect Unit)
    -> Routed action (C.Action d c n)
    -> Covered NoodleError (model /\ Network d c n)
    -> Covered NoodleError (model /\ Network d c n)
        /\ Effect (AndThen (Routed action (C.Action d c n)))


type ViewF d c n model view
    = Covered NoodleError (model /\ Network d c n) -> view


type MinimalViewF d c n view
    = Covered NoodleError (Network d c n) -> view


type Minimal d c n view
    = CoveredUI NoodleError (C.Action d c n) (Network d c n) view


make
    :: forall d c n action model view
     . Toolkit d c n
    -> UpdateF d c n action model view
    -> ViewF d c n model view
    -> Renderer d c n action model view
make toolkit updateF =
    UI.makeWithPush
        updateF'
    >>> UI.mapFSM (FSM.joinWith appendErrors)
    where
        updateF' pushAction action@(FromCore coreAction) coveredModel =
            let
                (uiModel /\ coreModel) = recover coveredModel
                coveredCoreModel /\ coreEffects =
                    C.apply toolkit (pushAction <<< FromCore) coreAction
                        $ appendErrors coveredModel
                        $ carry coreModel
                coveredModel' =
                    (\coreModel -> uiModel /\ coreModel)
                        <$> coveredCoreModel
                nextModel /\ uiEffects =
                    updateF pushAction action $ appendErrors coveredModel coveredModel'
                nextEffects = (map FromCore <$> coreEffects) <> uiEffects
            in
                nextModel /\ nextEffects
        updateF' pushAction action coveredModel =
            updateF pushAction action coveredModel


makeMinimal
    :: forall d c n view
     . Toolkit d c n
    -> MinimalViewF d c n view
    -> Minimal d c n view
makeMinimal toolkit =
    UI.makeWithPush
        (C.apply toolkit)
    >>> UI.mapFSM (FSM.joinWith appendErrors)
