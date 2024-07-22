module Noodle.Fn.Generic.Updates where

import Prelude


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))

import Noodle.Id (InputR, OutputR)


data InputChange
    = SingleInput InputR -- TODO: HoldsInput
    | AllInputs
    -- TODO: add Hot / Cold


data OutputChange
    = SingleOutput OutputR -- TODO: HoldsOutput
    | AllOutputs


data ChangeFocus
    = Everything
    | StateChange
    | AllInputsChange
    | InputChange InputR -- TODO: HoldsInput
    | AllOutputsChange
    | OutputChange OutputR -- TODO: HoldsOutput


instance Show ChangeFocus where
    show :: ChangeFocus -> String
    show =
        case _ of
            Everything -> "all"
            StateChange -> "state"
            AllInputsChange -> "inputs"
            InputChange inputR -> "input " <> show inputR -- reflect' inputR
            AllOutputsChange -> "outputs"
            OutputChange outputR -> "output " <> show outputR -- reflect' outputR


inputChangeToMaybe :: InputChange -> Maybe InputR
inputChangeToMaybe (SingleInput iid) = Just iid
inputChangeToMaybe AllInputs = Nothing


outputChangeToMaybe :: OutputChange -> Maybe OutputR
outputChangeToMaybe (SingleOutput oid) = Just oid
outputChangeToMaybe AllOutputs = Nothing


type PreUpdatesRow state inputs outputs = (Boolean /\ state) /\ (Boolean /\ InputChange /\ inputs) /\ (Boolean /\ OutputChange /\ outputs)
type PostUpdatesRow state inputs outputs = ChangeFocus /\ PreUpdatesRow state inputs outputs
type FocusedUpdate state inputs outputs = ChangeFocus /\ state /\ inputs /\ outputs