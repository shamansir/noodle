module Example.Toolkit.Minimal.Node.ModifiesPatch where

import Prelude


import Effect (Effect)

import Type.Data.List (type (:>))
import Type.Data.List.Extra (TNil)
import Data.Tuple.Nested ((/\), type (/\))

import Control.Monad.State.Class (modify_) as State

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
import Noodle.Repr.HasFallback (class HasFallback)

import Example.Toolkit.Minimal.ChRepr (MinimalVRepr)


_modifiesPatch :: NId.Family "modifiesPatch"
_modifiesPatch  = NId.Family


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


newtype State = State ({ intVal :: Int, strVal :: String } /\ String)


derive newtype instance Eq State
derive newtype instance Show State


type Shape   = Noodle.Shape Inlets Outlets
type Process = Noodle.Process State InletsRow OutletsRow MinimalVRepr Effect
type Node    = Noodle.Node   "modifiesPatch" State InletsRow OutletsRow MinimalVRepr Effect
type Family  = Noodle.Family "modifiesPatch" State InletsRow OutletsRow MinimalVRepr Effect
type F       = Noodle.F      "modifiesPatch" State InletsRow OutletsRow MinimalVRepr Effect


defaultI :: Record InletsRow
defaultI = { a : 0, b : 0 }
defaultO :: Record OutletsRow
defaultO = { sum : 0 }
defaultState :: State
defaultState = State $ { intVal : 0, strVal : "" } /\ "o"


a_in    = Noodle.Inlet :: _ "a"
b_in    = Noodle.Inlet :: _ "b"

sum_out = Noodle.Outlet :: _ "sum"


family :: Family
family =
    Family.make
        _modifiesPatch
        defaultState
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
    State.modify_
        \(State ({ intVal, strVal } /\ s)) -> State $
            { intVal : intVal + (a + b)
            , strVal : show (a + b) <> "*" <> strVal
            }
            /\
            (s <> "+" <> show (a + b))
    Fn.send sum_out $ a + b


instance HasFallback State where
    fallback = defaultState