module Test.Files.CodeGenTest.Input.Hydra.Array.Ease where

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

{- Generated by Noodle Codegen from Hydra NDF file. Group :: array. Family :: ease.

[[ : array : ease :: <arr:Values {%%%%} -> ease:Ease {LIN E}> => arr:Value {VA %%%%} /-| arr::HT.VArray <arr> <ease> |-/ ]] (#82) -}

_ease :: NId.Family "ease"
_ease = NId.Family

type Inlets = (I "arr" Hot HT.Values :> I "ease" Hot HT.Ease :> TNil) :: Noodle.Inlets
type Outlets = (O "arr" HT.Value :> TNil) :: Noodle.Outlets
type InletsRow = (arr :: HT.Values, ease :: HT.Ease)
type OutletsRow = (arr :: HT.Value)
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "ease" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "ease" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "ease" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI = { arr: HT.Values [], ease: HT.Linear }

defaultO :: Record OutletsRow
defaultO = { arr: HT.VArray (HT.Values []) HT.Linear }

defaultSt :: HW.WrapRepr
defaultSt = HW.Value HT.None

_in_arr = Noodle.Inlet :: _ "arr"
_in_ease = Noodle.Inlet :: _ "ease"
_out_arr = Noodle.Outlet :: _ "arr"

family :: Family
family = Family.make _ease defaultSt (Noodle.Shape :: Shape) defaultI defaultO easeP

makeNode :: Effect Node
makeNode = Family.spawn family

easeP :: Process
easeP = do
  arr <- Fn.receive _in_arr
  ease <- Fn.receive _in_ease
  Fn.send _out_arr $ HT.VArray arr ease
