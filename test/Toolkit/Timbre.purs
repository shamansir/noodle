module Rpd.Test.Toolkit.Timbre
    ( toolkit
    , Data
    )
    where

import Prelude

import Data.List ((:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Rpd.Def as R
import Rpd.Toolkit as T


data WaveKind
    = Sin
    | Saw
    | Tri
    | Pulse
    | Fami


data SoundState
    = Playing
    | Stopped


data Data
    = Value Number
    | Wave WaveKind
    | Sound SoundState


toolkit :: T.Toolkit Data
toolkit =
    { id : "timbre"
    , name : "Timbre"
    , nodeDefs :
        T.defs
            $ ("number" /\ numNode)
            : ("osc" /\ oscNode)
            : ("plot" /\ plotNode)
            : ("play" /\ playNode)
            : List.Nil
    , channelDefs : T.noDefs
    }


numNode :: R.NodeDef Data
numNode =
    { name : "Number"
    , inletDefs : List.Nil
    , outletDefs
        : numOutlet
        : List.Nil
    , process : R.FlowThrough -- TODO
    }


waveNode :: R.NodeDef Data
waveNode =
    { name : "Wave"
    , inletDefs
        : List.Nil
    , outletDefs
        : waveOutlet
        : List.Nil
    , process : R.FlowThrough -- TODO
    }


oscNode :: R.NodeDef Data
oscNode =
    { name : "Oscillator"
    , inletDefs
        : waveInlet
        : freqInlet
        : List.Nil
    , outletDefs
        : soundOutlet
        : List.Nil
    , process : R.FlowThrough -- TODO
    }


plotNode :: R.NodeDef Data
plotNode =
    { name : "Plot"
    , inletDefs
        : soundInlet
        : List.Nil
    , outletDefs
        : List.Nil
    , process : R.FlowThrough -- TODO
    }


playNode :: R.NodeDef Data
playNode =
    { name : "Play"
    , inletDefs
        : soundInlet
        : List.Nil
    , outletDefs
        : List.Nil
    , process : R.FlowThrough -- TODO
    }


waveInlet :: R.InletDef Data
waveInlet =
    { label : "wave"
    , default : Just default
    , accept : Just isWave
    }
    where
        default = Wave Sin
        isWave (Wave _) = true
        isWave _ = false


freqInlet :: R.InletDef Data
freqInlet =
    { label : "freq"
    , default : Just default
    , accept : Just isFreq
    }
    where
        default = Value 440.0
        isFreq (Value _) = true
        isFreq _ = false


soundInlet :: R.InletDef Data
soundInlet =
    { label : "sound"
    , default : Just default
    , accept : Just isSound
    }
    where
        default = Sound Stopped
        isSound (Sound _) = true
        isSound _ = false


numOutlet :: R.OutletDef Data
numOutlet =
    { label : "num" -- TODO: check if output is number somehow
    }


soundOutlet :: R.OutletDef Data
soundOutlet =
    { label : "sound" -- TODO: check if output is sound somehow
    }


waveOutlet :: R.OutletDef Data
waveOutlet =
    { label : "wave" -- TODO: check if output is sound somehow
    }
