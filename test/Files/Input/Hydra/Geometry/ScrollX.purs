module Test.Files.CodeGenTest.Hydra.Geometry.ScrollX where

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

_scrollX :: NId.Family "scrollX"
_scrollX = NId.Family

type Inlets =
  (I "what" Hot HT.Texture :> I "scrollX" Hot HT.Value :> I "speed" Hot HT.Value :> TNil) ::
    Noodle.Inlets

type Outlets = (O "out" HT.Texture :> TNil) :: Noodle.Outlets
type InletsRow = (what :: HT.Texture, scrollX :: HT.Value, speed :: HT.Value)
type OutletsRow = (out :: HT.Texture)
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "scrollX" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "scrollX" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "scrollX" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI = { what: HT.Empty, scrollX: HT.Number 0.5, speed: HT.Number 1.0 }

defaultO :: Record OutletsRow
defaultO = { out: HT.Empty }

_in_what = Noodle.Inlet :: _ "what"
_in_scrollX = Noodle.Inlet :: _ "scrollX"
_in_speed = Noodle.Inlet :: _ "speed"
_out_out = Noodle.Outlet :: _ "out"

family :: Family
family = Family.make _scrollX (HW.Value HT.None) (Noodle.Shape :: Shape) defaultI defaultO scrollXP

makeNode :: Effect Node
makeNode = Family.spawn family

scrollXP :: Process
scrollXP = do
  what <- Fn.receive _in_what
  scrollX <- Fn.receive _in_scrollX
  speed <- Fn.receive _in_speed
  HT.Geometry what $ HT.GScrollX { scrollX, speed }