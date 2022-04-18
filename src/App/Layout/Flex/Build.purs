module App.Layout.Flex.Build where

import Prelude


import Data.Tuple.Nested (type (/\), (/\))

import App.Layout.Flex.Axis
import App.Layout.Flex.Rule (Rule(..))


flex :: forall s a. Array (s /\ a) -> Axis s a
flex = make


flex2 :: forall s a. Array (s /\ Axis s a) -> Axis2 s a
flex2 = make


flex3 :: forall s a. Array (s /\ Axis s (Axis s a)) -> Axis3 s a
flex3 = make


flex4 :: forall s a. Array (s /\ Axis s (Axis s (Axis s a))) -> Axis4 s a
flex4 = make


horz :: forall s a. Array (s /\ a) -> Axis s a
horz = flex


vert :: forall s a. Array (s /\ Axis s a) -> Axis2 s a
vert = flex2


horz' :: forall s a. Array (s /\ Axis s (Axis s a)) -> Axis3 s a
horz' = flex3


vert' :: forall s a. Array (s /\ Axis s (Axis s (Axis s a))) -> Axis4 s a
vert' = flex3