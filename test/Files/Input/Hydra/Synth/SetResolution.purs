module Test.Files.CodeGenTest.Hydra.Synth.SetResolution where

import Prelude

import Effect (Effect)
import Hydra.Repr.Wrap (WrapRepr)
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

{- Generated by Noodle Codegen from Hydra NDF file. -}

_setResolution :: NId.Family "setResolution"
_setResolution = NId.Family

type Inlets = (I "width" Hot HT.Value :> I "height" Hot HT.Value :> TNil) :: Noodle.Inlets
type Outlets = (O "out" HT.TODO :> TNil) :: Noodle.Outlets
type InletsRow = (width :: HT.Value, height :: HT.Value)
type OutletsRow = (out :: HT.TODO)
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "setResolution" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "setResolution" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "setResolution" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI =
  { width: Error { source: "None", error: "Expected space" }
  , height: Error { source: "None", error: "Expected space" }
  }

defaultO :: Record OutletsRow
defaultO = { out: HT.TODO }

_in_width = Noodle.Inlet :: _ "width"
_in_height = Noodle.Inlet :: _ "height"
_out_out = Noodle.Outlet :: _ "out"

family :: Family
family = Family.make _setResolution (HW.Value HT.None) (Noodle.Shape :: Shape) defaultI defaultO
  setResolutionP

makeNode :: Effect Node
makeNode = Family.spawn family

setResolutionP :: Process
setResolutionP = pure unit