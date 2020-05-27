module Noodle.Render.Renderer where

import Prelude

import Effect (Effect)

import Data.Covered
import Data.List (List)
import Data.Tuple.Nested ((/\), type (/\))

import FSM (joinWith) as FSM

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
        /\ List (Effect (Routed action (C.Action d c n)))


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
        updateF' push action@(FromCore coreAction) coveredModel =
            let
                (uiModel /\ coreModel) = recover coveredModel
                coveredCoreModel /\ coreEffects =
                    C.apply toolkit (push <<< FromCore) coreAction
                        $ appendErrors coveredModel
                        $ carry coreModel
                coveredModel' =
                    (\coreModel -> uiModel /\ coreModel)
                        <$> coveredCoreModel
                nextModel /\ uiEffects =
                    updateF push action $ appendErrors coveredModel coveredModel'
                nextEffects = (map FromCore <$> coreEffects) <> uiEffects
            in
                nextModel /\ nextEffects
        updateF' push action coveredModel =
            updateF push action coveredModel


makeMinimal
    :: forall d c n view
     . Toolkit d c n
    -> MinimalViewF d c n view
    -> Minimal d c n view
makeMinimal toolkit =
    UI.makeWithPush
        (C.apply toolkit)
    >>> UI.mapFSM (FSM.joinWith appendErrors)
