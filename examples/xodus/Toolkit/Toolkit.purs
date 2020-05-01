module Xodus.Toolkit where

import Prelude (($), identity)

import Data.Maybe (Maybe(..))
import Data.Map (singleton) as Map
import Data.Tuple.Nested ((/\))

import Noodle.Toolkit as T
import Noodle.Util (type (/->))

import Xodus.Toolkit.Value
import Xodus.Toolkit.Channel
import Xodus.Toolkit.Node
import Xodus.Toolkit.Render.Html as RenderHtml


htmlRenderer = RenderHtml.renderer


toolkit :: T.Toolkit Value Channel Node
toolkit =
    T.Toolkit (T.ToolkitName "xodus") nodes
    where
        nodes NodeListNode = T.emptyNode
        nodes BangNode = bangNode
