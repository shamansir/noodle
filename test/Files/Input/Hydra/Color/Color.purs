module Test.Files.CodeGenTest.Hydra.Color.Color where

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

_color :: NId.Family "color"
_color = NId.Family

type Inlets =
  ( I "what" Hot HT.Texture :> I "r" Hot HT.Value :> I "g" Hot HT.Value :> I "b" Hot HT.Value
      :> I "a" Hot HT.Value
      :> TNil
  ) :: Noodle.Inlets

type Outlets = (O "out" HT.Texture :> TNil) :: Noodle.Outlets
type InletsRow = (what :: HT.Texture, r :: HT.Value, g :: HT.Value, b :: HT.Value, a :: HT.Value)
type OutletsRow = (out :: HT.Texture)
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "color" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "color" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "color" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI =
  { what: HT.Empty, r: HT.Number 1.0, g: HT.Number 1.0, b: HT.Number 1.0, a: HT.Number 1.0 }

defaultO :: Record OutletsRow
defaultO = { out: HT.Empty }

_in_what = Noodle.Inlet :: _ "what"
_in_r = Noodle.Inlet :: _ "r"
_in_g = Noodle.Inlet :: _ "g"
_in_b = Noodle.Inlet :: _ "b"
_in_a = Noodle.Inlet :: _ "a"
_out_out = Noodle.Outlet :: _ "out"

family :: Family
family = Family.make _color (HW.Value HT.None) (Noodle.Shape :: Shape) defaultI defaultO colorP

makeNode :: Effect Node
makeNode = Family.spawn family

colorP :: Process
colorP = do
  what <- Fn.receive _in_what
  r <- Fn.receive _in_r
  g <- Fn.receive _in_g
  b <- Fn.receive _in_b
  a <- Fn.receive _in_a
  Fn.send _out_out $ HT.Filter what $ HT.Color { r, g, b, a }