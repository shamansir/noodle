module Test.MyToolkit.Node.Sum where

import Prelude


import Effect (Effect)

import Noodle.Id (Family(..)) as NId
import Noodle.Fn.Shape (I, O, type (/.), type (\.), Hot, Cold, IS, OS)
import Noodle.Fn.Shape (Shape(..), Inlets, Outlets, Inlet(..), Outlet(..)) as Noodle
import Noodle.Fn.Process (Process) as Noodle
import Noodle.Fn.Process (receive, send) as Fn
import Noodle.Node (Node) as Noodle
import Noodle.Node (make) as Node
import Noodle.Toolkit.Families (Family, F) as Noodle
import Noodle.Toolkit.Families (make, spawn) as Family

import Test.MyToolkit.Repr (ISRepr)



_sum :: NId.Family "sum"
_sum  = NId.Family


type Inlets =
    (  I "a" Hot Int
    /. I "b" Hot Int
    /. IS
    ) :: Noodle.Inlets

type Outlets =
    (  O "sum" Int
    \. OS
    ) :: Noodle.Outlets

type InletsRow =
    ( a :: Int, b :: Int )

type OutletsRow =
    ( sum :: Int )


type Shape   = Noodle.Shape Inlets Outlets
type Process = Noodle.Process Unit InletsRow OutletsRow ISRepr Effect
type Node    = Noodle.Node   "sum" Unit InletsRow OutletsRow ISRepr Effect
type Family  = Noodle.Family "sum" Unit InletsRow OutletsRow ISRepr Effect
type F       = Noodle.F      "sum" Unit InletsRow OutletsRow ISRepr Effect


defaultI :: Record InletsRow
defaultI = { a : 0, b : 0 }
defaultO :: Record OutletsRow
defaultO = { sum : 0 }


a_in    = Noodle.Inlet :: _ "a"
b_in    = Noodle.Inlet :: _ "b"
c_in    = Noodle.Inlet :: _ "c"

sum_out = Noodle.Outlet :: _ "sum"


family :: Process -> Family
family =
    Family.make
        _sum
        unit
        (Noodle.Shape :: Shape)
        defaultI
        defaultO


makeNode :: Process -> Effect Node
makeNode =
    family >>> Family.spawn


sumBoth :: Process
sumBoth = do
    a <- Fn.receive a_in
    b <- Fn.receive b_in
    Fn.send sum_out $ a + b