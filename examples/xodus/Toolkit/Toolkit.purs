module Xodus.Toolkit where

import Noodle.Toolkit as T
import Noodle.Render.Html (ToolkitRenderer) as R

import Xodus.Toolkit.Value (Value)
import Xodus.Toolkit.Channel (Channel)
import Xodus.Toolkit.Node
import Xodus.Toolkit.Render.Html as RenderHtml


htmlRenderer :: R.ToolkitRenderer Value Channel Node
htmlRenderer = RenderHtml.renderer


toolkit :: T.Toolkit Value Channel Node
toolkit =
    T.Toolkit (T.ToolkitName "xodus") nodes
    where
        nodes NodeListNode = T.emptyNode
        nodes ConnectNode = connectNode
        nodes DatabasesNode = databaseNode
