module Example.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (singleton, insert, empty, lookup) as Map
import Data.Tuple.Nested ((/\), type (/\))
import Data.Exists


import Rpd.Network (Network)
import Rpd.Network (empty) as Network
import Rpd.API.Action as Action
import Rpd.API.Action.Apply (apply) as Action
import Rpd.API.Action.Sequence ((</>))
import Rpd.Render.Html (make, init) as HtmlRenderer
import Rpd.Render.Html.VDom as VDom
import Rpd.Toolkit as T


import Example.Network (recipe) as Network
import Example.Toolkit (toolkit, htmlRenderer) as ExampleToolkit


network :: forall d c n. Network d c n
network = Network.empty "aaa"


main :: Effect Unit
main = do
    VDom.embed'
        "#app"
        (HtmlRenderer.make ExampleToolkit.htmlRenderer ExampleToolkit.toolkit)
        (HtmlRenderer.init network /\ network)

    -- _ /\ { stop } <-
    --     Actions.runFolding
    --         ExampleToolkit.toolkit
    --         (Network.empty "network")
    --         $ Network.recipe
    --             </> Actions.do_
    --                 \nw ->
    --                 VDom.embed'
    --                     "#app"
    --                     (htmlRenderer ExampleToolkit.htmlRenderer)
    --                     ExampleToolkit.toolkit
    --                     nw
    -- _ <- stop
    pure unit
