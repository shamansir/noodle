module Test.Files.CodeGenTest.Input.Hydra.Extsource.InitStream where

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

{- Generated by Noodle Codegen from Hydra NDF file. Group :: extsource. Family :: initStream.

[[  extsource : initStream :: <src:SourceN -> todo:TODO {TODO}> => <> ]] (#79) -}

_initStream :: NId.Family "initStream"
_initStream = NId.Family

type Inlets = (I "src" Hot HT.SourceN :> I "todo" Hot HT.TODO :> TNil) :: Noodle.Inlets
type Outlets = TNil :: Noodle.Outlets
type InletsRow = (src :: HT.SourceN, todo :: HT.TODO)
type OutletsRow = ()
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "initStream" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "initStream" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "initStream" HW.WrapRepr InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI = { src: HT.Source0, todo: HT.TODO }

defaultO :: Record OutletsRow
defaultO = {}

defaultSt :: HW.WrapRepr
defaultSt = HW.Value HT.None

_in_src = Noodle.Inlet :: _ "src"
_in_todo = Noodle.Inlet :: _ "todo"

family :: Family
family = Family.make _initStream defaultSt (Noodle.Shape :: Shape) defaultI defaultO initStreamP

makeNode :: Effect Node
makeNode = Family.spawn family

initStreamP :: Process
initStreamP = pure unit
