module TensorFlow.Toolkit where

import Noodle.Toolkit as T
import Noodle.Render.Html (ToolkitRenderer) as R

import TensorFlow.Toolkit.Value (Value)
import TensorFlow.Toolkit.Channel (Channel)
import TensorFlow.Toolkit.Node
import TensorFlow.Toolkit.Render.Html as RenderHtml


htmlRenderer :: R.ToolkitRenderer Value Channel Node
htmlRenderer = RenderHtml.renderer


toolkit :: T.Toolkit Value Channel Node
toolkit =
    T.Toolkit (T.ToolkitName "tensoflow") nodes
    where
        nodes BangNode = bangNode
        nodes NodeListNode = T.emptyNode
        nodes InputLayerNode = inputLayerNode
        nodes AddNode = addNode
        nodes TfModelNode = tfModelNode
