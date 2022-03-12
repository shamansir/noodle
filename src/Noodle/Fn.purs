module Noodle.Fn
    ( Fn, Fn', make, make'
    , run
    , module StatefulExports
    )
    where


import Prelude


import Data.Newtype (class Newtype, unwrap)

import Data.Array as Array
import Data.Bifunctor (lmap, rmap, bimap)
import Data.Functor.Invariant (class Invariant)
import Data.Maybe (Maybe)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))

import Effect (Effect)
import Effect.Aff (Aff)

import Noodle.Fn.Process (ProcessM)
import Noodle.Fn.Process as Process
import Noodle.Fn.Transfer (Receive, Send)
import Noodle.Fn.Transfer as T
import Noodle.Fn.Stateful (Fn, Fn', make, make', run) as Stateful
import Noodle.Fn.Stateful (Name) as Fn
import Noodle.Fn.Stateful
            ( Name
            , InputId, OutputId
            , in_, _in, out_, _out
            , mapInputs, mapOutputs, mapInputsAndOutputs
            , mapInputsIds, mapOutputsIds, mapInputsAndOutputsIds
            , findInput, findOutput
            , shapeOf, dimensions, dimensionsBy
            , name
            )
            as StatefulExports


{-
    - `i` -> input ID
    - `ii` -> input info (i.e. channel)
    - `o` -> output ID
    - `oo` -> output info (i.e. channel)
    - `state` -> function state (to be shared / reused)
    - `m` -> monad where the function will be run
    - `d` -> data pass through the inputs/outputs
-}
type Fn' i ii o oo m d = Stateful.Fn' i ii o oo Unit m d


type Fn i o m d = Stateful.Fn i o Unit m d


{- Creating -}


make :: forall i o m d. Fn.Name -> Array i -> Array o -> ProcessM i o Unit d m Unit -> Fn i o m d
make = Stateful.make


make' :: forall i ii o oo m d. Fn.Name -> Array (i /\ ii) -> Array (o /\ oo) -> ProcessM i o Unit d m Unit -> Fn' i ii o oo m d
make' = Stateful.make'


{- Running -}


run :: forall i ii o oo d. Ord i => d -> Send o d -> Receive i d -> Fn' i ii o oo Aff d -> Aff Unit
run default =
    Stateful.run default unit