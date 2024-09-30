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
import Test.MyToolkit.Repr (ISRepr)
import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil)
import Data.String (length) as String

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

left_in = Noodle.Inlet :: _ "left"
right_in = Noodle.Inlet :: _ "right"
out_out = Noodle.Outlet :: _ "out"
len_out = Noodle.Outlet :: _ "len"

family :: Family
family = Family.make _concat unit (Noodle.Shape :: Shape) defaultI defaultO concatP

makeNode :: Effect Node
makeNode = Family.spawn family

concatP :: Process
concatP = do
  left <- Fn.receive left_in
  right <- Fn.receive right_in
  let concatenated = left <> right
  Fn.send out_out concatenated
  Fn.send len_out $ String.length concatenated
