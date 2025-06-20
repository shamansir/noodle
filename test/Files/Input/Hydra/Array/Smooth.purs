module Test.Files.CodeGenTest.Input.Hydra.Array.Smooth where

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

{- Generated by Noodle Codegen from Hydra NDF file. Group :: array. Family :: smooth.

[[ : array : smooth :: <arr:Values {%%%%} -> smooth:Value {N 1.0}> => arr:Value {VA %%%%} /-| arr::HT.VArray <arr> $ HT.Smooth <smooth> |-/ ]] (#81) -}

_smooth :: NId.Family "smooth"
_smooth = NId.Family

type Inlets = (I "arr" Hot HT.Values :> I "smooth" Hot HT.Value :> TNil) :: Noodle.Inlets
type Outlets = (O "arr" HT.Value :> TNil) :: Noodle.Outlets
type InletsRow = (arr :: HT.Values, smooth :: HT.Value)
type OutletsRow = (arr :: HT.Value)
type Shape = Noodle.Shape Inlets Outlets
newtype State = State Unit
type Process = Noodle.Process State InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "smooth" State InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "smooth" State InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "smooth" State InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI = { arr: HT.Values [], smooth: HT.Number 1.0 }

defaultO :: Record OutletsRow
defaultO = { arr: HT.VArray (HT.Values []) (HT.Ease HT.Linear) }

defaultSt :: State
defaultSt = State unit

_in_arr = Noodle.Inlet :: _ "arr"
_in_smooth = Noodle.Inlet :: _ "smooth"
_out_arr = Noodle.Outlet :: _ "arr"

family :: Family
family = Family.make _smooth defaultSt (Noodle.Shape :: Shape) defaultI defaultO smoothP

makeNode :: Effect Node
makeNode = Family.spawn family

smoothP :: Process
smoothP = do
  arr <- Fn.receive _in_arr
  smooth <- Fn.receive _in_smooth
  Fn.send _out_arr $ HT.VArray arr $ HT.Smooth smooth

instance HasFallback State where
  fallback = defaultSt

derive instance Newtype State _
