module Test.Files.CodeGenTest.Input.Hydra.Color.Luma where

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

{- Generated by Noodle Codegen from Hydra NDF file. Group :: color. Family :: luma.

[[ : color : luma :: <what:Texture {EMP T} -> threshold:Value {N 0.5} -> tolerance:Value {N 0.1}> => out:Texture {EMP T} /-| out::HT.Filter <what> $ HT.Luma { <threshold>, <tolerance> } |-/ ]] (#36) -}

_luma :: NId.Family "luma"
_luma = NId.Family

type Inlets =
  (I "what" Hot HT.Texture :> I "threshold" Hot HT.Value :> I "tolerance" Hot HT.Value :> TNil) ::
    Noodle.Inlets

type Outlets = (O "out" HT.Texture :> TNil) :: Noodle.Outlets
type InletsRow = (what :: HT.Texture, threshold :: HT.Value, tolerance :: HT.Value)
type OutletsRow = (out :: HT.Texture)
type Shape = Noodle.Shape Inlets Outlets
newtype State = State Unit
type Process = Noodle.Process State InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "luma" State InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "luma" State InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "luma" State InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI = { what: HT.Empty, threshold: HT.Number 0.5, tolerance: HT.Number 0.1 }

defaultO :: Record OutletsRow
defaultO = { out: HT.Empty }

defaultSt :: State
defaultSt = State unit

_in_what = Noodle.Inlet :: _ "what"
_in_threshold = Noodle.Inlet :: _ "threshold"
_in_tolerance = Noodle.Inlet :: _ "tolerance"
_out_out = Noodle.Outlet :: _ "out"

family :: Family
family = Family.make _luma defaultSt (Noodle.Shape :: Shape) defaultI defaultO lumaP

makeNode :: Effect Node
makeNode = Family.spawn family

lumaP :: Process
lumaP = do
  what <- Fn.receive _in_what
  threshold <- Fn.receive _in_threshold
  tolerance <- Fn.receive _in_tolerance
  Fn.send _out_out $ HT.Filter what $ HT.Luma { threshold, tolerance }

instance HasFallback State where
  fallback = defaultSt

derive instance Newtype State _
