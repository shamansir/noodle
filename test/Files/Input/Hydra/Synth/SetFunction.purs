module Test.Files.CodeGenTest.Hydra.Synth.SetFunction where

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

_setFunction :: NId.Family "setFunction"
_setFunction = NId.Family

type Inlets = (I "fn" Hot HT.GlslFn :> TNil) :: Noodle.Inlets
type Outlets = (O "out" HT.TODO :> TNil) :: Noodle.Outlets
type InletsRow = (fn :: HT.GlslFn)
type OutletsRow = (out :: HT.TODO)
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "setFunction" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "setFunction" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "setFunction" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI = { fn: HW.Value HT.None }

defaultO :: Record OutletsRow
defaultO = { out: HT.TODO }

_in_fn = Noodle.Inlet :: _ "fn"
_out_out = Noodle.Outlet :: _ "out"

family :: Family
family = Family.make _setFunction (HW.Value HT.None) (Noodle.Shape :: Shape) defaultI defaultO
  setFunctionP

makeNode :: Effect Node
makeNode = Family.spawn family

setFunctionP :: Process
setFunctionP = pure unit