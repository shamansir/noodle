module StarterTk.Spreads.Nspread where

import Prelude

import Demo.Toolkit.Starter.Repr (StarterRepr)
import Effect (Effect)
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
import Data.Tuple.Nested ((/\), type (/\))
import Demo.Toolkit.Starter.Repr as PR

{- Generated by Noodle Codegen from NDF file. Group :: spreads. Family :: nspread.

[[  spreads : nspread :: <min:Number {#/-150.0} -> max:Number {#/150.0} -> count:Number {#/26}> => spread:SpreadN ]] (#15) -}

_nspread :: NId.Family "nspread"
_nspread = NId.Family

type Inlets =
  (I "min" Hot Number :> I "max" Hot Number :> I "count" Hot Number :> TNil) :: Noodle.Inlets

type Outlets = (O "spread" (PR.Spread Number) :> TNil) :: Noodle.Outlets
type InletsRow = (min :: Number, max :: Number, count :: Number)
type OutletsRow = (spread :: PR.Spread Number)
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process StarterRepr InletsRow OutletsRow StarterRepr Effect
type Node = Noodle.Node "nspread" StarterRepr InletsRow OutletsRow StarterRepr Effect
type Family = Noodle.Family "nspread" StarterRepr InletsRow OutletsRow StarterRepr Effect
type F = Noodle.F "nspread" StarterRepr InletsRow OutletsRow StarterRepr Effect

defaultI :: Record InletsRow
defaultI = { min: -150.0, max: 150.0, count: 26.0 }

defaultO :: Record OutletsRow
defaultO = { spread: PR.Spread [] }

_in_min = Noodle.Inlet :: _ "min"
_in_max = Noodle.Inlet :: _ "max"
_in_count = Noodle.Inlet :: _ "count"
_out_spread = Noodle.Outlet :: _ "spread"

family :: Family
family = Family.make _nspread PR.VNone (Noodle.Shape :: Shape) defaultI defaultO nspreadP

makeNode :: Effect Node
makeNode = Family.spawn family

nspreadP :: Process
nspreadP = pure unit
