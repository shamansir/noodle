module Example.Toolkit where

import Data.Map (singleton) as Map
import Data.Tuple.Nested ((/\))

import Rpd.Toolkit as T
import Rpd.Util (type (/->))

import Example.Toolkit.Value
import Example.Toolkit.Channel
import Example.Toolkit.Nodes
import Example.Toolkit.Render.Html as RenderHtml


toolkit :: T.Toolkit Value Channel
toolkit =
    T.Toolkit
        { name : T.ToolkitName "example"
        , nodes
        , render :
            Map.singleton (T.RendererAlias "html") RenderHtml.renderer
        }
    where
        nodes :: T.NodeDefAlias /-> T.NodeDef Value Channel
        nodes =
            T.nodes
                [ "random" /\ randomNode
                ]
