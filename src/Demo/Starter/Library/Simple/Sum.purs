module StarterTk.Library.Simple.Sum where

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

{- Generated by Noodle Codegen from NDF file. Group :: simple. Family :: sum.

[[  simple : sum :: <a:Number -> b:Number -> c:Number> => sum:Number {#/0.0} ]] (#5) -}

_sum :: NId.Family "sum"
_sum = NId.Family

type Inlets = (I "a" Hot Number :> I "b" Hot Number :> I "c" Hot Number :> TNil) :: Noodle.Inlets
type Outlets = (O "sum" Number :> TNil) :: Noodle.Outlets
type InletsRow = (a :: Number, b :: Number, c :: Number)
type OutletsRow = (sum :: Number)
type Shape = Noodle.Shape Inlets Outlets
type Process = Noodle.Process Unit InletsRow OutletsRow ValueRepr Effect
type Node = Noodle.Node "sum" Unit InletsRow OutletsRow ValueRepr Effect
type Family = Noodle.Family "sum" Unit InletsRow OutletsRow ValueRepr Effect
type F = Noodle.F "sum" Unit InletsRow OutletsRow ValueRepr Effect

defaultI :: Record InletsRow
defaultI = { a: 0.0, b: 0.0, c: 0.0 }

defaultO :: Record OutletsRow
defaultO = { sum: 0.0 }

_in_a = Noodle.Inlet :: _ "a"
_in_b = Noodle.Inlet :: _ "b"
_in_c = Noodle.Inlet :: _ "c"
_out_sum = Noodle.Outlet :: _ "sum"

family :: Family
family = Family.make _sum unit (Noodle.Shape :: Shape) defaultI defaultO sumP

makeNode :: Effect Node
makeNode = Family.spawn family

sumP :: Process
sumP = do
    a <- Noodle.receive _in_a
    b <- Noodle.receive _in_b
    c <- Noodle.receive _in_c
    Noodle.send _out_sum $ a + b + c
