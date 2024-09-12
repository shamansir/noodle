module Test.MyToolkit.Node.Sample where

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

import Test.MyToolkit.Repr (ISRepr)


_sample :: NId.Family "sample"
_sample  = NId.Family


type Inlets =
    (  I "foo" Hot Int
    :> I "c" Hot Int
    :> I "bar" Cold String
    :> TNil
    ) :: Noodle.Inlets

type Outlets =
    (  O "foo" String
    :> O "bar" Int
    :> TNil
    ) :: Noodle.Outlets

type InletsRow =
    ( foo :: Int
    , c :: Int
    , bar :: String
    )

type OutletsRow =
    ( foo :: String
    , bar :: Int
    )


type Shape   = Noodle.Shape Inlets Outlets
type Process = Noodle.Process Unit InletsRow OutletsRow ISRepr Effect
type Node    = Noodle.Node   "sample" Unit InletsRow OutletsRow ISRepr Effect
type Family  = Noodle.Family "sample" Unit InletsRow OutletsRow ISRepr Effect
type F       = Noodle.F      "sample" Unit InletsRow OutletsRow ISRepr Effect


defaultI :: Record InletsRow
defaultI = { foo : 1, c : 2, bar : "5" }
defaultO :: Record OutletsRow
defaultO = { foo : "1", bar : 12 }


foo_in  = Noodle.Inlet :: _ "foo"
bar_in  = Noodle.Inlet :: _ "bar"
c_in    = Noodle.Inlet :: _ "c"

foo_out = Noodle.Outlet :: _ "foo"
bar_out = Noodle.Outlet :: _ "bar"


family :: Process -> Family
family =
    Family.make
        _sample
        unit
        (Noodle.Shape :: Shape)
        defaultI
        defaultO


makeNode :: Process -> Effect Node
makeNode =
    family >>> Family.spawn


combineAll :: Process
combineAll = do
    foo <- Fn.receive foo_in
    bar <- Fn.receive bar_in
    c <- Fn.receive c_in
    -- liftEffect $ Console.log $ "run : <foo> " <> show foo <> " <bar> " <> show bar <> " <c> " <> show c
    Fn.send foo_out $ show (foo + c) <> bar
    Fn.send bar_out $ foo - c