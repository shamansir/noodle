module Rpd.Test.Toolkit.Timbre
    ( toolkit
    , Data
    , Channel
    )
    where

import Prelude

import Data.List ((:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Rpd.Process as R
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


data Channel
    = CValue
    | CWave
    | CSound


instance timbreChannel :: T.Channel Channel Data where
    default CValue = Value 0.0
    default CWave = Wave Sin
    default CSound = Sound Stopped
    accept _ _ = true
    adapt _ = identity


toolkit :: T.Toolkit Channel Data
toolkit =
    T.Toolkit
        { name : T.ToolkitName "Timbre"
        , nodes :
            T.nodes
                [ "number" /\ numNode
                , "osc" /\ oscNode
                , "plot" /\ plotNode
                , "play" /\ playNode
                ]
                -- $ ("number" /\ numNode)
                -- : ("osc" /\ oscNode)
                -- : ("plot" /\ plotNode)
                -- : ("play" /\ playNode)
                -- : List.Nil
        }


numNode :: T.NodeDef Channel Data
numNode =
    T.NodeDef
        { inlets : T.inlets []
        , outlets
            : T.outlets
                [ ( "num" /\ CValue )
                ]
        , process : R.Withhold -- TODO
        }


waveNode :: T.NodeDef Channel Data
waveNode =
    T.NodeDef
        { inlets
            : T.inlets []
        , outlets
            : T.outlets
                [ ( "wave" /\ CWave )
                ]
        , process : R.Withhold -- TODO
        }


oscNode :: T.NodeDef Channel Data
oscNode =
    T.NodeDef
        { inlets
            : T.inlets
                [ "freq" /\ CValue
                , "wave" /\ CWave
                ]
        , outlets
            : T.outlets
                [ "sound" /\ CSound
                ]
        , process : R.Withhold -- TODO
        }


plotNode :: T.NodeDef Channel Data
plotNode =
    T.NodeDef
        { inlets
            : T.inlets
                [ "sound" /\ CSound ]
        , outlets
            : T.outlets []
        , process : R.Withhold -- TODO
        }


playNode :: T.NodeDef Channel Data
playNode =
    T.NodeDef
        { inlets
            : T.inlets
                [ "sound" /\ CSound ]
        , outlets
            : T.outlets []
        , process : R.Withhold -- TODO
        }
