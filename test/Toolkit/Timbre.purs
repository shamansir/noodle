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
    , process : Nothing -- TODO
    }


waveNode :: R.NodeDef Data
waveNode =
    { name : "Wave"
    , inletDefs
        : List.Nil
    , outletDefs
        : waveOutlet
        : List.Nil
    , process : Nothing -- TODO
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
    , process : Nothing -- TODO
    }


plotNode :: R.NodeDef Data
plotNode =
    { name : "Plot"
    , inletDefs
        : soundInlet
        : List.Nil
    , outletDefs
        : List.Nil
    , process : Nothing -- TODO
    }


playNode :: R.NodeDef Data
playNode =
    { name : "Play"
    , inletDefs
        : soundInlet
        : List.Nil
    , outletDefs
        : List.Nil
    , process : Nothing -- TODO
    }


waveInlet :: R.InletDef Data
waveInlet =
    { label : "wave"
    , default : pure $ Wave Sin
    , accept : pure isWave
    }



freqInlet :: R.InletDef Data
freqInlet =
    { label : "freq"
    , default : pure $ Value 440.0
    , accept : pure isNumber
    }


soundInlet :: R.InletDef Data
soundInlet =
    { label : "sound"
    , default : pure $ Sound Stopped
    , accept : pure isSound
    }



numOutlet :: R.OutletDef Data
numOutlet =
    { label : "num"
    , accept : pure isNumber
    }


soundOutlet :: R.OutletDef Data
soundOutlet =
    { label : "sound"
    , accept : pure isSound
    }


waveOutlet :: R.OutletDef Data
waveOutlet =
    { label : "wave"
    , accept : pure isWave
    }


isWave (Wave _) = true
isWave _ = false


isNumber (Value _) = true
isNumber _ = false


isSound (Sound _) = true
isSound _ = false
