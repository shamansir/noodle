module Test.MyToolkit.Node.Stateful where

import Prelude


import Effect (Effect)
import Control.Monad.State.Class (modify_) as State

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



_stateful :: NId.Family "stateful"
_stateful  = NId.Family


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


type State = String


type Shape   = Noodle.Shape Inlets Outlets
type Process = Noodle.Process State InletsRow OutletsRow ISRepr Effect
type Node    = Noodle.Node   "stateful" State InletsRow OutletsRow ISRepr Effect
type Family  = Noodle.Family "stateful" State InletsRow OutletsRow ISRepr Effect
type F       = Noodle.F      "stateful" State InletsRow OutletsRow ISRepr Effect


defaultI :: Record InletsRow
defaultI = { a : 0, b : 0 }
defaultO :: Record OutletsRow
defaultO = { sum : 0 }


a_in    = Noodle.Inlet :: _ "a"
b_in    = Noodle.Inlet :: _ "b"
c_in    = Noodle.Inlet :: _ "c"

sum_out = Noodle.Outlet :: _ "sum"


family :: Family
family =
    Family.make
        _stateful
        "x"
        (Noodle.Shape :: Shape)
        defaultI
        defaultO
        sumAndStore


makeNode :: Effect Node
makeNode =
    Family.spawn family


sumAndStore :: Process
sumAndStore = do
    a <- Fn.receive a_in
    b <- Fn.receive b_in
    State.modify_ (\s -> s <> "-" <> show (a + b))
    Fn.send sum_out $ a + b