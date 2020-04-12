module Rpd.Render.Html.NodeList where

import Prelude (($), (<$>))

import Rpd.Render.Html (View)
import Rpd.Render.Html (core) as R
import Rpd.Render.Atom (class Atom, labelOf)
import Rpd.API.Action (Action(..), RequestAction(..)) as A
import Rpd.Path (ToPatch) as P

import Spork.Html as H


render :: forall d c n. Atom n => P.ToPatch -> Array n -> View d c n
render patchPath nodes =
    H.div [ H.classes [ "rpd-node-list" ] ]
        $ createNodeButton <$> nodes
        where
            createNodeButton n =
                H.div
                    [ H.classes [ "rpd-node-list-button" ]
                    , H.onClick $ H.always_ $ R.core
                        $ A.Request $ A.ToAddNextNode patchPath n
                    ]
                    [ H.text $ labelOf n ] -- TODO: add toolkit name. may be use `n -> String`
