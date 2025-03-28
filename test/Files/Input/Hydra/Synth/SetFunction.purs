module Test.Files.CodeGenTest.Input.Hydra.Synth.SetFunction where

import Prelude

import Data.Newtype (class Newtype)
import Effect (Effect)
import HydraTk.Repr.Wrap (WrapRepr(..))
import Noodle.Fn.Process as Fn
import Noodle.Fn.Process as Noodle
import Noodle.Fn.Shape (I, O)
import Noodle.Fn.Shape as Noodle
import Noodle.Fn.Shape.Temperament (Cold, Hot)
import Noodle.Id as NId
import Noodle.Node as Noodle
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Toolkit.Families as Noodle
import Noodle.Toolkit.Family as Family
import Noodle.Toolkit.Family as Noodle
import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil)
import HydraTk.Types as HT
import HydraTk.Repr.Wrap as HW
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
newtype State = State Unit
type Process = Noodle.Process State InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "setFunction" State InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "setFunction" State InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "setFunction" State InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI = { fn: HT.GlslFn { kind: HT.FnSrc, code: HT.GlslFnCode "", fn: Sig ("" /\ [] /\ []) } }

defaultO :: Record OutletsRow
defaultO = {}

defaultSt :: State
defaultSt = State unit

_in_fn = Noodle.Inlet :: _ "fn"

family :: Family
family = Family.make _setFunction defaultSt (Noodle.Shape :: Shape) defaultI defaultO setFunctionP

makeNode :: Effect Node
makeNode = Family.spawn family

setFunctionP :: Process
setFunctionP = {- EMPTY PROCESS -}
    pure unit

instance HasFallback State where
  fallback = defaultSt

derive instance Newtype State _
