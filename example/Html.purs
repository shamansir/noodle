module Example.Html where

import Prelude

import Effect (Effect)

import Data.Map (Map)
import Data.Map (singleton, insert, empty, lookup) as Map
import Data.Tuple.Nested ((/\), type (/\))
import Data.Exists

import Rpd.Network (empty) as Network
import Rpd.Renderer.Html (htmlRenderer)
import Rpd.Renderer.Html.VDom as VDom
import Rpd.Toolkit as T

import Example.Network (network)
import Example.Toolkit (toolkit)


main :: Effect Unit
main =
    let
        toolkits = T.Toolkits
            $ Map.singleton (T.ToolkitName "example")
            $ T.mkToolkitE toolkit
        renderer =
            case toolkit of
                (T.Toolkit t) -> Map.lookup (T.RendererAlias "html") t.render
    in
        -- FIXME: toolkits are passed to the rendering engine twice, that should be separated
        VDom.embed' "#app" (htmlRenderer toolkits) toolkits network
