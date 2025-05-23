module HydraTk.Library.Out.Out where

import Prelude

import Data.Newtype (class Newtype)
import Effect (Effect)
import Control.Monad.State (put) as State
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
import HydraTk.Lang.Command as HL
import HydraTk.Repr.Wrap as HW
import Data.Tuple.Nested ((/\))

{- Generated by Noodle Codegen from Hydra NDF file. Group :: out. Family :: out.

[[  out : out :: <what:Texture {EMP T} -> target:RenderTarget {ALL 4}> => <> ]] (#93) -}

_out :: NId.Family "out"
_out = NId.Family

type Inlets = (I "what" Hot HT.Texture :> I "target" Hot HT.RenderTarget :> TNil) :: Noodle.Inlets
type Outlets = TNil :: Noodle.Outlets
type InletsRow = (what :: HT.Texture, target :: HT.RenderTarget)
type OutletsRow = ()
type Shape = Noodle.Shape Inlets Outlets
newtype State = State HL.Command
type Process = Noodle.Process State InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "out" State InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "out" State InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "out" State InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI = { what: HT.Empty, target: HT.Four }

defaultO :: Record OutletsRow
defaultO = {}

defaultSt :: State
defaultSt = State HL.Unknown

_in_what = Noodle.Inlet :: _ "what"
_in_target = Noodle.Inlet :: _ "target"

family :: Family
family = Family.make _out defaultSt (Noodle.Shape :: Shape) defaultI defaultO outP

makeNode :: Effect Node
makeNode = Family.spawn family

outP :: Process
outP = do
    what <- Fn.receive _in_what
    target <- Fn.receive _in_target
    let
      outputN = case target of
        HT.Four -> HT.Output0
        (HT.Output outputN) -> outputN
    State.put $ State $ HL.Chain outputN what


instance HasFallback State where
  fallback = defaultSt

derive instance Newtype State _
