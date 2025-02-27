module Test.Files.CodeGenTest.Input.Hydra.Audio.Fft where

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

{- Generated by Noodle Codegen from Hydra NDF file. Group :: audio. Family :: fft.

[[ : audio : fft :: <bin:AudioBin {@0}> => fft:Value {0 V} /-| fft::HT.Fft <bin> |-/ ]] (#85) -}

_fft :: NId.Family "fft"
_fft = NId.Family

type Inlets = (I "bin" Hot HT.AudioBin :> TNil) :: Noodle.Inlets
type Outlets = (O "fft" HT.Value :> TNil) :: Noodle.Outlets
type InletsRow = (bin :: HT.AudioBin)
type OutletsRow = (fft :: HT.Value)
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "fft" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "fft" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "fft" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI = { bin: HT.AudioBin 0 }

defaultO :: Record OutletsRow
defaultO = { fft: HT.None }

defaultSt :: HW.WrapRepr
defaultSt = HW.Value HT.None

_in_bin = Noodle.Inlet :: _ "bin"
_out_fft = Noodle.Outlet :: _ "fft"

family :: Family
family = Family.make _fft defaultSt (Noodle.Shape :: Shape) defaultI defaultO fftP

makeNode :: Effect Node
makeNode = Family.spawn family

fftP :: Process
fftP = do
  bin <- Fn.receive _in_bin
  Fn.send _out_fft $ HT.Fft bin
