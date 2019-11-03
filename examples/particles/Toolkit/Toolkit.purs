module Example.Toolkit where

import Prelude (($), identity)

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


toolkit :: T.Toolkit Value Channel Node
toolkit =
    T.Toolkit (T.ToolkitName "particles") nodes
    where
        nodes NodeListNode = T.emptyNode
        nodes RandomNode = randomNode
        nodes FillNode = fillNode
        nodes TimeNode = timeNode
        nodes CanvasNode = canvasNode
        nodes ShapeNode = shapeNode
        nodes SpreadNode = spreadNode
        nodes PairNode = pairNode
        nodes _ = T.emptyNode


-- instance exampleChannel :: T.Channels Value Channel where
--     default _ = Bang
--     accept _ _ = true
--     adapt _ = identity
