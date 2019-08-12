module Example.Html where

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
import Rpd.API.Action.Sequence (run', run_, LastStep(..), ErrorHandler(..)) as Actions
import Rpd.Renderer.Html (htmlRenderer)
import Rpd.Renderer.Html.VDom as VDom
import Rpd.Toolkit as T


import Example.Network (recipe) as Network
import Example.Toolkit (toolkit, htmlRenderer) as ExampleToolkit


main :: Effect Unit
main =
    -- VDom.embed'
    --     "#app"
    --     (htmlRenderer ExampleToolkit.htmlRenderer)
    --     ExampleToolkit.toolkit
    --     (Network.empty "aaa")
    Actions.run_
        ExampleToolkit.toolkit
        (Network.empty "network")
        (Actions.ErrorHandler $ log <<< show)
        (Actions.LastStep
            \nw ->
                VDom.embed'
                    "#app"
                    (htmlRenderer ExampleToolkit.htmlRenderer)
                    ExampleToolkit.toolkit
                    nw
        )
        Network.recipe
