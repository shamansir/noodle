module App.Layout.Flex.Build where

import Prelude


import Data.Tuple.Nested (type (/\), (/\))

import App.Layout.Flex


horz :: forall s a. Array (s /\ a) -> Flex s a
horz = make


vert :: forall s a. Array (s /\ Flex s a) -> Flex2 s a
vert = make


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