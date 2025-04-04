module HydraTk.Library.Source.Gradient where

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

{- Generated by Noodle Codegen from Hydra NDF file. Group :: source. Family :: gradient.

[[ : source : gradient :: <speed:Value> => out:Texture {EMP T} /-| out::HT.Start $ HT.From $ HT.Gradient { <speed> } |-/ ]] (#17) -}

_gradient :: NId.Family "gradient"
_gradient = NId.Family

type Inlets = (I "speed" Hot HT.Value :> TNil) :: Noodle.Inlets
type Outlets = (O "out" HT.Texture :> TNil) :: Noodle.Outlets
type InletsRow = (speed :: HT.Value)
type OutletsRow = (out :: HT.Texture)
type Shape = Noodle.Shape Inlets Outlets
newtype State = State Unit
type Process = Noodle.Process State InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "gradient" State InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "gradient" State InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "gradient" State InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI = { speed: HT.None }

defaultO :: Record OutletsRow
defaultO = { out: HT.Empty }

defaultSt :: State
defaultSt = State unit

_in_speed = Noodle.Inlet :: _ "speed"
_out_out = Noodle.Outlet :: _ "out"

family :: Family
family = Family.make _gradient defaultSt (Noodle.Shape :: Shape) defaultI defaultO gradientP

makeNode :: Effect Node
makeNode = Family.spawn family

gradientP :: Process
gradientP = do
  speed <- Fn.receive _in_speed
  Fn.send _out_out $ HT.Start $ HT.From $ HT.Gradient { speed }

instance HasFallback State where
  fallback = defaultSt

derive instance Newtype State _
