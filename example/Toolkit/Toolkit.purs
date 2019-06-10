module Example.Toolkit where

import Prelude (($))

import Data.Maybe (Maybe(..))
import Data.Map (singleton) as Map
import Data.Tuple.Nested ((/\))

import Rpd.Toolkit as T
import Rpd.Util (type (/->))

import Example.Toolkit.Value
import Example.Toolkit.Channel
import Example.Toolkit.Nodes
import Example.Toolkit.Render.Html as RenderHtml


    -- let
    --     toolkits = T.Toolkits
    --         $ Map.singleton (T.ToolkitName "example")
    --         $ T.mkToolkitE toolkit
    -- in
    --     VDom.embed' "#app" htmlRenderer toolkits network

--htmlRenderer :: T.ToolkitRenderer Value Channel (View Value) (Command Value)
htmlRenderer = RenderHtml.renderer


toolkit :: T.Toolkit Value Channel
toolkit =
    T.Toolkit (T.ToolkitName "example") nodes
    where
        nodes (T.NodeDefAlias "random") = Just randomNode
        nodes _ = Nothing
