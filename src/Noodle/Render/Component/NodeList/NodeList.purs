module Noodle.Render.Component.NodeList where

import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Render.Action (RoutedAction)
import Noodle.Render.Atom (class Atom)
import Noodle.Path (ToPatch) as P

import UI.Component (Component) as UI
import UI.Component (makePassing) as Component


type NodeList d c n v = UI.Component (RoutedAction d c n) (P.ToPatch /\ Array n) v


make
    :: forall d c n v
     . Atom n
    => (P.ToPatch /\ Array n -> v (RoutedAction d c n))
    -> P.ToPatch /\ Array n
    -> NodeList d c n v
make = Component.makePassing


-- makeHtml:: forall d c n. Atom n => P.ToPatch /\ Array n -> NodeList d c n H.Html
-- makeHtml = make renderHtml

