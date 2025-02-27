module Test.Files.CodeGenTest.Input.Hydra.Audio.SetSmooth where

import Prelude

import Effect (Effect)
import Hydra.Repr.Wrap (WrapRepr(..))
import Noodle.Fn.Process as Fn
import Noodle.Fn.Process as Noodle
import Noodle.Fn.Shape (I, O)
import Noodle.Fn.Shape as Noodle
import Noodle.Fn.Shape.Temperament (Cold, Hot)
import Noodle.Id as NId
import Noodle.Node as Noodle
import Noodle.Toolkit.Families as Noodle
import Noodle.Toolkit.Family as Family
import Noodle.Toolkit.Family as Noodle
import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil)
import Hydra.Types as HT
import Hydra.Repr.Wrap as HW
import Data.Tuple.Nested ((/\))

{- Generated by Noodle Codegen from Hydra NDF file. Group :: audio. Family :: setSmooth.

[[ : audio : setSmooth :: <audio:Audio {SIL} -> smooth:Value {N 0.4}> => <> ]] (#86) -}

_setSmooth :: NId.Family "setSmooth"
_setSmooth = NId.Family

type Inlets = (I "audio" Hot HT.AudioSource :> I "smooth" Hot HT.Value :> TNil) :: Noodle.Inlets
type Outlets = TNil :: Noodle.Outlets
type InletsRow = (audio :: HT.AudioSource, smooth :: HT.Value)
type OutletsRow = ()
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "setSmooth" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "setSmooth" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "setSmooth" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI = { audio: HT.Silence, smooth: HT.Number 0.4 }

defaultO :: Record OutletsRow
defaultO = {}

defaultSt :: HW.WrapRepr
defaultSt = HW.Value HT.None

_in_audio = Noodle.Inlet :: _ "audio"
_in_smooth = Noodle.Inlet :: _ "smooth"

family :: Family
family = Family.make _setSmooth defaultSt (Noodle.Shape :: Shape) defaultI defaultO setSmoothP

makeNode :: Effect Node
makeNode = Family.spawn family

setSmoothP :: Process
setSmoothP = pure unit
