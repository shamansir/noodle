module Example.Toolkit.Minimal.Node.SampleHC where

import Prelude

import Effect (Effect)

import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil)

import Noodle.Id (Family(..)) as NId
import Noodle.Fn.Shape.Temperament (Hot, Cold)
import Noodle.Fn.Shape (I, O)
import Noodle.Fn.Shape (Shape(..), Inlets, Outlets, Inlet(..), Outlet(..)) as Noodle
import Noodle.Fn.Process (Process) as Noodle
import Noodle.Fn.Process (receive, send) as Fn
import Noodle.Node (Node) as Noodle
import Noodle.Toolkit.Family (Family) as Noodle
import Noodle.Toolkit.Family (make, spawn) as Family
import Noodle.Toolkit.Families (F) as Noodle

import Example.Toolkit.Minimal.Repr (MinimalVRepr)
import Example.Toolkit.Minimal.Node.Sample as Src


_sampleHC :: NId.Family "sampleHC"
_sampleHC  = NId.Family


type Inlets =
    (  I "foo" Hot Int
    :> I "c" Hot Int
    :> I "bar" Cold String -- it's cold, unlike in the source
    :> TNil
    ) :: Noodle.Inlets

type Outlets =
    (  O "foo" String
    :> O "bar" Int
    :> TNil
    ) :: Noodle.Outlets


type InletsRow = Src.InletsRow

type OutletsRow = Src.OutletsRow


type Shape   = Noodle.Shape Inlets Outlets
type Process = Src.Process
type Node    = Noodle.Node   "sampleHC" Unit InletsRow OutletsRow MinimalVRepr Effect
type Family  = Noodle.Family "sampleHC" Unit InletsRow OutletsRow MinimalVRepr Effect
type F       = Noodle.F      "sampleHC" Unit InletsRow OutletsRow MinimalVRepr Effect


defaultI :: Record InletsRow
defaultI = Src.defaultI
defaultO :: Record OutletsRow
defaultO = Src.defaultO


foo_in  = Src.foo_in :: _ "foo"
bar_in  = Src.bar_in :: _ "bar"
c_in    = Src.c_in   :: _ "c"

foo_out = Src.foo_out :: _ "foo"
bar_out = Src.bar_out :: _ "bar"


family :: Family
family =
    family' combineAll


family' :: Process -> Family
family' =
    Family.make
        _sampleHC
        unit
        (Noodle.Shape :: Shape)
        defaultI
        defaultO


makeNode :: Effect Node
makeNode =
    Family.spawn family


makeNode' :: Process -> Effect Node
makeNode' =
    family' >>> Family.spawn


combineAll :: Process
combineAll = Src.combineAll