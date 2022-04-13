module App.Layout.Flex.Build where

import Prelude


import Data.Tuple.Nested (type (/\), (/\))

import App.Layout.Flex


horz :: forall s a. Array (s /\ a) -> HBox s a
horz = Horz


vert :: forall s a. Array (s /\ HBox s a) -> VBox s a
vert = Vert


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