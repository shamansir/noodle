module Test.Files.CodeGenTest.Input.Hydra.Feed.Expression where

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

{- Generated by Noodle Codegen from Hydra NDF file. Group :: feed. Family :: expression.

[[  feed : expression :: <> => out:Value {D /----/} ]] (#10) -}

_expression :: NId.Family "expression"
_expression = NId.Family

type Inlets = TNil :: Noodle.Inlets
type Outlets = (O "out" HT.Value :> TNil) :: Noodle.Outlets
type InletsRow = ()
type OutletsRow = (out :: HT.Value)
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "expression" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "expression" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "expression" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI = {}

defaultO :: Record OutletsRow
defaultO = { out: HT.Dep HT.NoAction }

defaultSt :: HW.WrapRepr
defaultSt = HW.Value HT.None

_out_out = Noodle.Outlet :: _ "out"

family :: Family
family = Family.make _expression defaultSt (Noodle.Shape :: Shape) defaultI defaultO expressionP

makeNode :: Effect Node
makeNode = Family.spawn family

expressionP :: Process
expressionP = pure unit
