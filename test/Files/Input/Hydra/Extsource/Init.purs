module Test.Files.CodeGenTest.Hydra.Extsource.Init where

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

_init :: NId.Family "init"
_init = NId.Family

type Inlets = (I "options" Hot HW.WrapRepr :> TNil) :: Noodle.Inlets
type Outlets = TNil :: Noodle.Outlets
type InletsRow = (options :: HW.WrapRepr)
type OutletsRow = ()
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "init" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "init" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "init" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI = { options: HW.Value HT.None }

defaultO :: Record OutletsRow
defaultO = {}

_in_options = Noodle.Inlet :: _ "options"

family :: Family
family = Family.make _init (HW.Value HT.None) (Noodle.Shape :: Shape) defaultI defaultO initP

makeNode :: Effect Node
makeNode = Family.spawn family

initP :: Process
initP = pure unit