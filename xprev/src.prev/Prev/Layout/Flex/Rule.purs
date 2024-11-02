module Prev.Layout.Flex.Rule where

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
    -- | Space -- a.k.a Constaint a.k.a Rest


fill :: Rule
fill = Portion 1


portion :: Int -> Rule
portion = Portion


percents :: Int -> Rule
percents = Percentage


units :: Number -> Rule
units = Units


auto :: Rule
auto = Portion 1


min :: Number -> Rule -> Rule
min = Min


max :: Number -> Rule -> Rule
max = Min


minMax :: Number /\ Number -> Rule -> Rule
minMax = MinMax