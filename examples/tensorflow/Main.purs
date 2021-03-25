module TensorFlow.Main where

import Prelude

import Effect (Effect)

import Data.Tuple.Nested ((/\), type (/\))


import Noodle.Network (Network)
import Noodle.Network (empty) as Network
import Noodle.API.Action.Sequence (pushAll)
import Noodle.Render.Html (make, init) as HtmlRenderer
import Noodle.Render.Html.VDom as VDom


import TensorFlow.Network (recipe) as TensorFlow
import TensorFlow.Toolkit (toolkit, htmlRenderer) as TensorFlowToolkit


network :: forall d c n. Network d c n
network = Network.empty "tensorflow"


main :: Effect Unit
main = do
    push <-
        VDom.embed'
            "#app"
            (HtmlRenderer.make TensorFlowToolkit.htmlRenderer TensorFlowToolkit.toolkit)
            (HtmlRenderer.init network /\ network)
    pushAll push TensorFlow.recipe
    pure unit
