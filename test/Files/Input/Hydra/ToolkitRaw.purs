module INPUT.Hydra.Gen.ToolkitRaw where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe(..), fromMaybe)

import Type.Proxy (Proxy(..))
import Type.Data.List.Extra (TNil, class Put)

import Noodle.Id (toolkitR, unsafeFamilyR) as Id
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit (empty, registerRaw) as Toolkit
import Noodle.Raw.Toolkit.Family (qmake) as Family
import Noodle.Repr.HasFallback (fallback) as Repr
import Noodle.Text.NdfFile.Types (EncodedType(..), EncodedValue(..)) as Ndf
import Noodle.Text.NdfFile.FamilyDef.Codegen (class ParseableRepr, toRepr, toDefault) as Ndf

import HydraTk.Repr.State (StateRepr)
import HydraTk.Repr.Wrap (WrapRepr)


foreign import data HYDRA :: ToolkitKey


toolkit :: Toolkit HYDRA TNil StateRepr WrapRepr Effect
toolkit =
    Toolkit.empty (Proxy :: _ HYDRA) (Id.toolkitR "Hydra")
    # (qregister "osc"
            [ { name : "frequency", tag : "Value", value : Just "N 60.0" }
            , { name : "sync", tag : "Value", value : Just "N 0.1" }
            , { name : "offset", tag : "Value", value : Nothing }
            ]
            [ { name : "out", tag : "Texture", value : Just "EMP T" }
            ]
            $ pure unit)
    where
        qregister family inlets outlets =
            Toolkit.registerRaw
            <<< Family.qmake
                (Id.unsafeFamilyR family)
                Repr.fallback -- FIXME
                rawDecode
                { inlets, outlets }
        rawDecode { tag, value } =
            value
                >>= (Ndf.EncodedValue >>> Ndf.toRepr (Ndf.EncodedType tag))
                # fromMaybe (Ndf.toDefault $ Ndf.EncodedType tag)