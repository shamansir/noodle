module Test.Files.CodeGenTest.Input.Hydra.Synth.Speed where

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

{- Generated by Noodle Codegen from Hydra NDF file. Group :: synth. Family :: speed.

[[  synth : speed :: <v:Value {N 1.0}> => <> ]] (#71) -}

_speed :: NId.Family "speed"
_speed = NId.Family

type Inlets = (I "v" Hot HT.Value :> TNil) :: Noodle.Inlets
type Outlets = TNil :: Noodle.Outlets
type InletsRow = (v :: HT.Value)
type OutletsRow = ()
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "speed" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "speed" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "speed" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI = { v: HT.Number 1.0 }

defaultO :: Record OutletsRow
defaultO = {}

defaultSt :: HW.WrapRepr
defaultSt = HW.Value HT.None

_in_v = Noodle.Inlet :: _ "v"

family :: Family
family = Family.make _speed defaultSt (Noodle.Shape :: Shape) defaultI defaultO speedP

makeNode :: Effect Node
makeNode = Family.spawn family

speedP :: Process
speedP = pure unit
