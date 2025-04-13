module Example.Toolkit.Minimal.Node.Sum where

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

import Example.Toolkit.Minimal.ChRepr (MinimalVRepr)


_sum :: NId.Family "sum"
_sum  = NId.Family


type Inlets =
    (  I "a" Hot Int
    :> I "b" Hot Int
    :> TNil
    ) :: Noodle.Inlets

type Outlets =
    (  O "sum" Int
    :> TNil
    ) :: Noodle.Outlets

type InletsRow =
    ( a :: Int, b :: Int )

type OutletsRow =
    ( sum :: Int )


type Shape   = Noodle.Shape Inlets Outlets
type Process = Noodle.Process Unit InletsRow OutletsRow MinimalVRepr Effect
type Node    = Noodle.Node   "sum" Unit InletsRow OutletsRow MinimalVRepr Effect
type Family  = Noodle.Family "sum" Unit InletsRow OutletsRow MinimalVRepr Effect
type F       = Noodle.F      "sum" Unit InletsRow OutletsRow MinimalVRepr Effect


defaultI :: Record InletsRow
defaultI = { a : 0, b : 0 }
defaultO :: Record OutletsRow
defaultO = { sum : 0 }


a_in    = Noodle.Inlet :: _ "a"
b_in    = Noodle.Inlet :: _ "b"

sum_out = Noodle.Outlet :: _ "sum"


family :: Family
family =
    family_
        defaultI
        defaultO
        sumBoth



family_ :: Record InletsRow -> Record OutletsRow -> Process -> Family
family_ =
    Family.make
        _sum
        unit
        (Noodle.Shape :: Shape)


family' :: Process -> Family
family' =
    family_
        defaultI
        defaultO


makeNode :: Effect Node
makeNode =
    Family.spawn family


makeNode_ :: Record InletsRow -> Record OutletsRow -> Process -> Effect Node
makeNode_ defaultI defaultO =
    family_ defaultI defaultO >>> Family.spawn


makeNode' :: Process -> Effect Node
makeNode' =
    family' >>> Family.spawn


sumBoth :: Process
sumBoth = do
    a <- Fn.receive a_in
    b <- Fn.receive b_in
    Fn.send sum_out $ a + b