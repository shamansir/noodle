module App.Layout.Flex.Rule where

import Prelude

import Data.Tuple.Nested (type (/\))

data Rule
    = Portion Int -- a.k.a Fill a.k.a Portion 1
    | Units Number -- a.k.a. Px or Units
    | Percentage Int
    -- | Cells Number
    | Min Number Rule
    | Max Number Rule
    | MinMax (Number /\ Number) Rule
    -- | TODO: Fn (a -> Rule a)