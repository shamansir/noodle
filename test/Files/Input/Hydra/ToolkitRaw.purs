module INPUT.Hydra.Gen.ToolkitRaw where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe(..))

import Type.Proxy (Proxy(..))
import Type.Data.List.Extra (TNil)

import Noodle.Id (toolkitR) as Id
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (empty) as Toolkit
import Noodle.Unsafe.QuickMake.RawToolkit (qregister) as Toolkit

import HydraTk.Repr.State (StateRepr)
import HydraTk.Repr.Wrap (WrapRepr)


foreign import data HYDRA :: ToolkitKey


toolkit :: Toolkit HYDRA TNil StateRepr WrapRepr Effect
toolkit =
    Toolkit.empty (Proxy :: _ HYDRA) (Id.toolkitR "Hydra")
    # (Toolkit.qregister "osc"
            [ { name : "frequency", tag : "Value", value : Just "N 60.0" }
            , { name : "sync", tag : "Value", value : Just "N 0.1" }
            , { name : "offset", tag : "Value", value : Nothing }
            ]
            [ { name : "out", tag : "Texture", value : Just "EMP T" }
            ]
            $ pure unit)
