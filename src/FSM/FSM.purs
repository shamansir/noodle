module FSM
    ( FSM(..)
    , Perform
    ) where

import Prelude (Unit)
import Effect (Effect)

import Data.Tuple.Nested ((/\), type (/\))
import Data.Either (Either)

import FSM.Covered (Covered)


type Perform action = Unit -> Effect (Array action)


data FSM model error action =
    FMM model (action -> Covered error model -> Either error model /\ Array (Perform action))
