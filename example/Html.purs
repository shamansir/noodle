module Example.Html where

import Prelude

import Effect (Effect)

import Rpd.Network (empty) as Network
import Rpd.Renderer.Html.Html (htmlRenderer)
import Rpd.Renderer.Html.VDom as VDom

main :: Effect Unit
main =
    VDom.embed "#app" ?wh htmlRenderer
        $ Network.empty "foo"
