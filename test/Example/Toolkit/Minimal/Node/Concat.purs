module Example.Toolkit.Minimal.Node.Concat where

import Prelude


import Effect (Effect)

import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil)

import Data.String (length) as String

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

import Example.Toolkit.Minimal.Repr (MinimalRepr)



_concat :: NId.Family "concat"
_concat  = NId.Family


type Inlets =
    (  I "left" Hot String
    :> I "right" Hot String
    :> TNil
    ) :: Noodle.Inlets

type Outlets =
    (  O "out" String
    :> O "len" Int
    :> TNil
    ) :: Noodle.Outlets

type InletsRow =
    ( left :: String, right :: String )

type OutletsRow =
    ( out :: String, len :: Int )


type Shape   = Noodle.Shape Inlets Outlets
type Process = Noodle.Process Unit InletsRow OutletsRow MinimalRepr Effect
type Node    = Noodle.Node   "concat" Unit InletsRow OutletsRow MinimalRepr Effect
type Family  = Noodle.Family "concat" Unit InletsRow OutletsRow MinimalRepr Effect
type F       = Noodle.F      "concat" Unit InletsRow OutletsRow MinimalRepr Effect


defaultI :: Record InletsRow
defaultI = { left : "", right : "" }
defaultO :: Record OutletsRow
defaultO = { out : "", len : 0 }


left_in  = Noodle.Inlet :: _ "left"
right_in = Noodle.Inlet :: _ "right"

out_out = Noodle.Outlet :: _ "out"
len_out = Noodle.Outlet :: _ "len"


family :: Family
family =
    Family.make
        _concat
        unit
        (Noodle.Shape :: Shape)
        defaultI
        defaultO
        concatP


makeNode :: Effect Node
makeNode =
    Family.spawn family


concatP :: Process
concatP = do
    left  <- Fn.receive left_in
    right <- Fn.receive right_in
    let concatenated = left <> right
    Fn.send out_out concatenated
    Fn.send len_out $ String.length concatenated