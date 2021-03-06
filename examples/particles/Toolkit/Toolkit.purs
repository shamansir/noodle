module Example.Toolkit where

import Prelude (($), identity)

import Data.Maybe (Maybe(..))
import Data.Map (singleton) as Map
import Data.Tuple.Nested ((/\))

import Noodle.Toolkit as T
import Noodle.Util (type (/->))

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
        nodes BangNode = bangNode
        nodes RandomNode = randomNode
        nodes NumberNode = numberNode
        nodes VectorNode = vectorNode
        nodes ColorNode = colorNode
        nodes FillNode = fillNode
        nodes MoveNode = moveNode
        nodes TimeNode = timeNode
        nodes CanvasNode = canvasNode
        nodes ShapeNode = shapeNode
        nodes SpreadNode = spreadNode
        nodes JoinNode = joinNode


-- instance exampleChannel :: T.Channels Value Channel where
--     default _ = Bang
--     accept _ _ = true
--     adapt _ = identity
