module Test.Files.CodeGenTest.Add where

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

_add :: NId.Family "add"
_add = NId.Family

type Inlets =
  (I "what" Hot HT.Texture :> I "with" Hot HT.Texture :> I "amount" Hot HT.Value :> TNil) ::
    Noodle.Inlets

type Outlets = (O "out" HT.Texture :> TNil) :: Noodle.Outlets
type InletsRow = (what :: HT.Texture, with :: HT.Texture, amount :: HT.Value)
type OutletsRow = (out :: HT.Texture)
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "add" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "add" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "add" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI = { what: HT.Empty, with: HT.Empty, amount: HT.Number 1.0 }

defaultO :: Record OutletsRow
defaultO = { out: HT.Empty }

_in_what = Noodle.Inlet :: _ "what"
_in_with = Noodle.Inlet :: _ "with"
_in_amount = Noodle.Inlet :: _ "amount"
_out_out = Noodle.Outlet :: _ "out"

family :: Family
family = Family.make _add (HW.Value HT.None) (Noodle.Shape :: Shape) defaultI defaultO addP

makeNode :: Effect Node
makeNode = Family.spawn family

addP :: Process
addP = do
  what <- Fn.receive _in_what
  with <- Fn.receive _in_with
  amount <- Fn.receive _in_amount
  Fn.send _out_out $ HT.BlendOf { what, with } $ HT.Add amount