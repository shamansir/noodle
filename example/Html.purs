module Example.Html where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (singleton, insert, empty, lookup) as Map
import Data.Tuple.Nested ((/\), type (/\))
import Data.Exists

import Rpd.Network (empty) as Network
import Rpd.Renderer.Html (htmlRenderer)
import Rpd.Renderer.Html.VDom as VDom
import Rpd.Toolkit as T

import Example.Network (network)
import Example.Toolkit (toolkit, htmlRenderer) as ExampleToolkit


main :: Effect Unit
main =
    let
        -- lookupFlipped :: forall k v. Ord k => Map k v -> k -> Maybe v
        -- lookupFlipped = flip Map.lookup
        toolkits =
            flip Map.lookup $ Map.singleton (T.ToolkitName "example") ExampleToolkit.toolkit
                --  T.mkToolkitE ExampleToolkit.toolkit
    in
        -- FIXME: toolkits are passed to the rendering engine twice, that should be separated
        VDom.embed' "#app" (htmlRenderer ExampleToolkit.htmlRenderer) toolkits network
