module Example.Html where

import Prelude

import Effect (Effect)

import Data.Map (Map)
import Data.Map (singleton, insert, empty) as Map
import Data.Tuple.Nested ((/\), type (/\))
import Data.Exists

import Rpd.Network (empty) as Network
import Rpd.Renderer.Html.Html (htmlRenderer)
import Rpd.Renderer.Html.VDom as VDom
import Rpd.Toolkit (Toolkits(..), ToolkitName(..), mkToolkitE) as T

import Example.Network (network)
import Example.Toolkit (toolkit)


main :: Effect Unit
main =
    let
        toolkits = T.Toolkits
            $ Map.singleton (T.ToolkitName "example")
            $ T.mkToolkitE toolkit
    in
        VDom.embed' "#app" htmlRenderer toolkits network
