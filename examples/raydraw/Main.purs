module RayDraw.Main where

import Prelude

import Effect (Effect)

import Data.Tuple.Nested ((/\), type (/\))


import Noodle.Network (Network)
import Noodle.Network (empty) as Network
import Noodle.API.Action.Sequence (pushAll)
import Noodle.Render.Html (make, init) as HtmlRenderer
import Noodle.Render.Html.VDom as VDom


import RayDraw.Network (recipe) as RayDraw
import RayDraw.Toolkit (toolkit, htmlRenderer) as RayDrawToolkit


network :: forall d c n. Network d c n
network = Network.empty "RayDraw"


main :: Effect Unit
main = do
    push <-
        VDom.embed'
            "#app"
            (HtmlRenderer.make RayDrawToolkit.htmlRenderer RayDrawToolkit.toolkit)
            (HtmlRenderer.init network /\ network)
    pushAll push RayDraw.recipe
    pure unit
