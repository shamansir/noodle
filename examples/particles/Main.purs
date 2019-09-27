module Example.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (singleton, insert, empty, lookup) as Map
import Data.Tuple.Nested ((/\), type (/\))
import Data.Exists


import Rpd.Network (empty) as Network
import Rpd.API.Action as Action
import Rpd.API.Action.Apply (apply) as Action
import Rpd.API.Action.Sequence (runFolding, do_) as Actions
import Rpd.API.Action.Sequence ((</>))
import Rpd.Renderer.Html (htmlRenderer)
import Rpd.Renderer.Html.VDom as VDom
import Rpd.Toolkit as T


import Example.Network (recipe) as Network
import Example.Toolkit (toolkit, htmlRenderer) as ExampleToolkit


main :: Effect Unit
main = do
    -- VDom.embed'
    --     "#app"
    --     (htmlRenderer ExampleToolkit.htmlRenderer)
    --     ExampleToolkit.toolkit
    --     (Network.empty "aaa")

    _ <-
        Actions.runFolding
            ExampleToolkit.toolkit
            (Network.empty "network")
            $ Network.recipe
                </> Actions.do_
                    \nw ->
                    VDom.embed'
                        "#app"
                        (htmlRenderer ExampleToolkit.htmlRenderer)
                        ExampleToolkit.toolkit
                        nw
    pure unit
