module Test.Files.CodeGenTest.Input.Hydra.Synth.SetFunction where

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
import Noodle.Fn.Signature (Signature(..))

{- Generated by Noodle Codegen from Hydra NDF file. Group :: synth. Family :: setFunction.

[[  synth : setFunction :: <fn:GlslFn> => <> ]] (#70) -}

_setFunction :: NId.Family "setFunction"
_setFunction = NId.Family

type Inlets = (I "fn" Hot HT.GlslFn :> TNil) :: Noodle.Inlets
type Outlets = TNil :: Noodle.Outlets
type InletsRow = (fn :: HT.GlslFn)
type OutletsRow = ()
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "setFunction" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "setFunction" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "setFunction" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI = { fn: HT.GlslFn { kind: HT.FnSrc, code: HT.GlslFnCode "", fn: Sig ("" /\ [] /\ []) } }

defaultO :: Record OutletsRow
defaultO = {}

defaultSt :: HW.WrapRepr
defaultSt = HW.Value HT.None

_in_fn = Noodle.Inlet :: _ "fn"

family :: Family
family = Family.make _setFunction defaultSt (Noodle.Shape :: Shape) defaultI defaultO setFunctionP

makeNode :: Effect Node
makeNode = Family.spawn family

setFunctionP :: Process
setFunctionP = pure unit
