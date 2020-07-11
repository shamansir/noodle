module Noodle.Render.Html.NodeList where

import Prelude (($), (<$>), (<<<), (>>>), flip)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))

import Noodle.Render.Action (RoutedAction)
import Noodle.Render.Action (core) as R
import Noodle.Render.Html (View)
import Noodle.Render.Atom (class Atom, labelOf)
import Noodle.API.Action (Action(..), RequestAction(..)) as A
import Noodle.Path (ToPatch) as P

import UI.Component (Component) as UI
import UI.Component (makePassing) as Component


import Spork.Html as H


type NodeList d c n v = UI.Component (RoutedAction d c n) (P.ToPatch /\ Array n) v


make
    :: forall d c n v
     . Atom n
    => (P.ToPatch /\ Array n -> v (RoutedAction d c n))
    -> P.ToPatch /\ Array n
    -> NodeList d c n v
make = Component.makePassing


makeHtml:: forall d c n. Atom n => P.ToPatch /\ Array n -> NodeList d c n H.Html
makeHtml = make renderHtml


renderHtml :: forall d c n. Atom n => P.ToPatch /\ Array n -> View d c n
renderHtml (patchPath /\ nodes) =
    H.div [ H.classes [ "noodle-node-list" ] ]
        $ createNodeButton <$> nodes
        where
            createNodeButton n =
                H.div
                    [ H.classes [ "noodle-node-list-button" ]
                    , H.onClick $ H.always_ $ R.core
                        $ A.Request $ A.ToAddNextNode patchPath n
                    ]
                    [ H.text $ labelOf n ] -- TODO: add toolkit name. may be use `n -> String`

