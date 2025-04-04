module HydraTk.Library.Extsource.Init where

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

{- Generated by Noodle Codegen from Hydra NDF file. Group :: extsource. Family :: init.

[[  extsource : init :: <options:SourceOptions> => <> ]] (#78) -}

_init :: NId.Family "init"
_init = NId.Family

type Inlets = (I "options" Hot HT.SourceOptions :> TNil) :: Noodle.Inlets
type Outlets = TNil :: Noodle.Outlets
type InletsRow = (options :: HT.SourceOptions)
type OutletsRow = ()
type Shape = Noodle.Shape Inlets Outlets
newtype State = State Unit
type Process = Noodle.Process State InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "init" State InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "init" State InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "init" State InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI = { options: HT.SourceOptions { src: HT.Canvas } }

defaultO :: Record OutletsRow
defaultO = {}

defaultSt :: State
defaultSt = State unit

_in_options = Noodle.Inlet :: _ "options"

family :: Family
family = Family.make _init defaultSt (Noodle.Shape :: Shape) defaultI defaultO initP

makeNode :: Effect Node
makeNode = Family.spawn family

initP :: Process
initP = {- EMPTY PROCESS -}
    pure unit

instance HasFallback State where
  fallback = defaultSt

derive instance Newtype State _
