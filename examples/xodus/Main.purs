module Xodus.Main where

import Prelude

import Effect (Effect)

import Data.Tuple.Nested ((/\), type (/\))


import Noodle.Network (Network)
import Noodle.Network (empty) as Network
import Noodle.API.Action.Sequence (pushAll)
import Noodle.Render.Model (init) as HtmlRenderer
import Noodle.Render.Html (make) as HtmlRenderer
import Noodle.Render.Html.VDom as VDom


import Xodus.Network (recipe) as Xodus
import Xodus.Toolkit (toolkit, htmlRenderer) as XodusToolkit


network :: forall d c n. Network d c n
network = Network.empty "xodus"


main :: Effect Unit
main = do
    push <-
        VDom.embed'
            "#app"
            (HtmlRenderer.make XodusToolkit.htmlRenderer XodusToolkit.toolkit)
            (HtmlRenderer.init network /\ network)
    pushAll push Xodus.recipe
    pure unit
