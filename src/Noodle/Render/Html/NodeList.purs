module Noodle.Render.Html.NodeList where

import Prelude (($), (<$>))
import Data.Newtype (wrap)
import Data.Tuple.Nested (type (/\))

import Noodle.Render.Renderer (Routed(..))
import Noodle.Render.Html (View, RoutedAction)
import Noodle.Render.Html (core, Action) as R
import Noodle.Render.Atom (class Atom, labelOf)
import Noodle.API.Action (Action(..), RequestAction(..)) as A
import Noodle.Path (ToPatch) as P
import UI (UI)


import Spork.Html as H


type NodeList d c n m = UI (RoutedAction d c n) (P.ToPatch /\ (Array n)) (m (RoutedAction d c n))
type NodeListHtml d c n = NodeList d c n H.Html


render :: forall d c n. Atom n => P.ToPatch -> Array n -> View d c n
render patchPath nodes =
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

