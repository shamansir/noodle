module Test.Files.CodeGenTest.Input.Hydra.Geometry.Repeat where

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

{- Generated by Noodle Codegen from Hydra NDF file. Group :: geometry. Family :: repeat.

[[ : geometry : repeat :: <what:Texture {EMP T} -> repeatX:Value {N 3.0} -> repeatY:Value {N 3.0} -> offsetX:Value {N 0.0} -> offsetY:Value {N 0.0}> => out:Texture {EMP T} /-| out::HT.Geometry <what> $ HT.GRepeat { <repeatX>, <repeatY>, <offsetX>, <offsetY> } |-/ ]] (#24) -}

_repeat :: NId.Family "repeat"
_repeat = NId.Family

type Inlets =
  ( I "what" Hot HT.Texture :> I "repeatX" Hot HT.Value :> I "repeatY" Hot HT.Value
      :> I "offsetX" Hot HT.Value
      :> I "offsetY" Hot HT.Value
      :> TNil
  ) :: Noodle.Inlets

type Outlets = (O "out" HT.Texture :> TNil) :: Noodle.Outlets
type InletsRow =
  ( what :: HT.Texture
  , repeatX :: HT.Value
  , repeatY :: HT.Value
  , offsetX :: HT.Value
  , offsetY :: HT.Value
  )

type OutletsRow = (out :: HT.Texture)
type Shape = Noodle.Shape Inlets Outlets
newtype State = State Unit
type Process = Noodle.Process State InletsRow OutletsRow WrapRepr Effect
type Node = Noodle.Node "repeat" State InletsRow OutletsRow WrapRepr Effect
type Family = Noodle.Family "repeat" State InletsRow OutletsRow WrapRepr Effect
type F = Noodle.F "repeat" State InletsRow OutletsRow WrapRepr Effect

defaultI :: Record InletsRow
defaultI =
  { what: HT.Empty
  , repeatX: HT.Number 3.0
  , repeatY: HT.Number 3.0
  , offsetX: HT.Number 0.0
  , offsetY: HT.Number 0.0
  }

defaultO :: Record OutletsRow
defaultO = { out: HT.Empty }

defaultSt :: State
defaultSt = State unit

_in_what = Noodle.Inlet :: _ "what"
_in_repeatX = Noodle.Inlet :: _ "repeatX"
_in_repeatY = Noodle.Inlet :: _ "repeatY"
_in_offsetX = Noodle.Inlet :: _ "offsetX"
_in_offsetY = Noodle.Inlet :: _ "offsetY"
_out_out = Noodle.Outlet :: _ "out"

family :: Family
family = Family.make _repeat defaultSt (Noodle.Shape :: Shape) defaultI defaultO repeatP

makeNode :: Effect Node
makeNode = Family.spawn family

repeatP :: Process
repeatP = do
  what <- Fn.receive _in_what
  repeatX <- Fn.receive _in_repeatX
  repeatY <- Fn.receive _in_repeatY
  offsetX <- Fn.receive _in_offsetX
  offsetY <- Fn.receive _in_offsetY
  Fn.send _out_out $ HT.Geometry what $ HT.GRepeat { repeatX, repeatY, offsetX, offsetY }

instance HasFallback State where
  fallback = defaultSt

derive instance Newtype State _
