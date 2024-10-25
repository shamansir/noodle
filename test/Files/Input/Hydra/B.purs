module Test.Files.CodeGenTest.B where

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

_b :: NId.Family "b"
_b = NId.Family

type Inlets =
  (I "what" Hot HT.Texture :> I "scale" Hot HT.Value :> I "offset" Hot HT.Value :> TNil) ::
    Noodle.Inlets

type Outlets = (O "out" HT.Texture :> TNil) :: Noodle.Outlets
type InletsRow = (what :: HT.Texture, scale :: HT.Value, offset :: HT.Value)
type OutletsRow = (out :: HT.Texture)
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "b" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "b" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "b" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI = { what: HT.Empty, scale: HT.Number 1.0, offset: HT.Number 0.0 }

defaultO :: Record OutletsRow
defaultO = { out: HT.Empty }

_in_what = Noodle.Inlet :: _ "what"
_in_scale = Noodle.Inlet :: _ "scale"
_in_offset = Noodle.Inlet :: _ "offset"
_out_out = Noodle.Outlet :: _ "out"

family :: Family
family = Family.make _b (HW.Value HT.None) (Noodle.Shape :: Shape) defaultI defaultO bP

makeNode :: Effect Node
makeNode = Family.spawn family

bP :: Process
bP = do
  what <- Fn.receive _in_what
  scale <- Fn.receive _in_scale
  offset <- Fn.receive _in_offset
  Fn.send _out_out $ HT.Filter what $ HT.B { scale, offset }
