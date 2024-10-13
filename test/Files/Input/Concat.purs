module Test.Files.CodeGenTest.Concat where

import Prelude

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
import Example.Toolkit.Minimal.Repr (ISRepr)
import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil)
import Data.String (length) as String
import Example.Toolkit.Minimal.Repr (ISRepr(..))

_concat :: NId.Family "concat"
_concat = NId.Family

type Inlets = (I "left" Hot String :> I "right" Hot String :> TNil) :: Noodle.Inlets
type Outlets = (O "out" String :> O "len" Int :> TNil) :: Noodle.Outlets
type InletsRow = (left :: String, right :: String)
type OutletsRow = (out :: String, len :: Int)
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process Unit InletsRow OutletsRow ISRepr Effect
type Node = Noodle.Node "concat" Unit InletsRow OutletsRow ISRepr Effect
type Family = Noodle.Family "concat" Unit InletsRow OutletsRow ISRepr Effect
type F = Noodle.F "concat" Unit InletsRow OutletsRow ISRepr Effect

defaultI :: Record InletsRow
defaultI = { left: "", right: "" }

defaultO :: Record OutletsRow
defaultO = { out: "", len: 0 }

_in_left = Noodle.Inlet :: _ "left"
_in_right = Noodle.Inlet :: _ "right"
_out_out = Noodle.Outlet :: _ "out"
_out_len = Noodle.Outlet :: _ "len"

family :: Family
family = Family.make _concat unit (Noodle.Shape :: Shape) defaultI defaultO concatP

makeNode :: Effect Node
makeNode = Family.spawn family

concatP :: Process
concatP = do
  left <- Fn.receive _in_left
  right <- Fn.receive _in_right
  let concatenated = left <> right
  Fn.send _out_out concatenated
  Fn.send _out_len $ String.length concatenated
