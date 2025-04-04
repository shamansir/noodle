module StarterTk.Library.P5.Shape where

import Prelude

import StarterTk.Repr.ChRepr (ValueRepr)
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
import StarterTk.Repr.ChRepr as VR
import Cli.Keys (NodeBoxKey)
import Blessed.Internal.BlessedOp (BlessedOp)
{- Generated by Noodle Codegen from NDF file. Group :: p5. Family :: shape.

[[  p5 : shape :: <> => shape:Shape {s/circle} ]] (#13) -}

_shape :: NId.Family "shape"
_shape = NId.Family

type Inlets = TNil :: Noodle.Inlets
type Outlets = (O "shape" VR.Shape :> TNil) :: Noodle.Outlets
type InletsRow = ()
type OutletsRow = (shape :: VR.Shape)
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process Unit InletsRow OutletsRow ValueRepr Effect
type Node = Noodle.Node "shape" Unit InletsRow OutletsRow ValueRepr Effect
type Family = Noodle.Family "shape" Unit InletsRow OutletsRow ValueRepr Effect
type F = Noodle.F "shape" Unit InletsRow OutletsRow ValueRepr Effect

defaultI :: Record InletsRow
defaultI = {}

defaultO :: Record OutletsRow
defaultO = { shape: VR.Circle }

_out_shape = Noodle.Outlet :: _ "shape"

family :: Family
family = Family.make _shape unit (Noodle.Shape :: Shape) defaultI defaultO shapeP

makeNode :: Effect Node
makeNode = Family.spawn family

shapeP :: Process
shapeP = pure unit
