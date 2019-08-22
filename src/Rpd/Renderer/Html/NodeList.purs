module Rpd.Renderer.Html.NodeList where

import Prelude (($), (<$>), class Show, show)

import Rpd.Renderer.Html (View)
import Rpd.Renderer.Html (core) as R
import Rpd.API.Action (Action(..), RequestAction(..)) as A
import Rpd.Path (ToPatch) as P

import Spork.Html as H

render :: forall d c n. Show n => P.ToPatch -> Array n -> View d c n
render patchPath nodes =
    H.div []
        $ createNodeButton <$> nodes
        where
            createNodeButton n =
                H.div
                    [ H.classes [ "rpd-node-list" ]
                    , H.onClick $ H.always_ $ R.core
                        $ A.Request $ A.ToAddNode patchPath (show n) n
                    ]
                    [ H.text $ show n ] -- TODO: add toolkit name. may be use `n -> String`
