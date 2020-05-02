module Noodle.Render.Html.NodeList where

import Prelude (($), (<$>))

import Noodle.Render.Html (View)
import Noodle.Render.Html (core) as R
import Noodle.Render.Atom (class Atom, labelOf)
import Noodle.API.Action (Action(..), RequestAction(..)) as A
import Noodle.Path (ToPatch) as P

import Spork.Html as H


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
