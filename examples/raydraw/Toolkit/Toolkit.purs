module RayDraw.Toolkit where

import Noodle.Toolkit as T
import Noodle.Render.Html (ToolkitRenderer) as R

import RayDraw.Toolkit.Value (Value)
import RayDraw.Toolkit.Channel (Channel)
import RayDraw.Toolkit.Node
import RayDraw.Toolkit.Render.Html as RenderHtml


htmlRenderer :: R.ToolkitRenderer Value Channel Node
htmlRenderer = RenderHtml.renderer


toolkit :: T.Toolkit Value Channel Node
toolkit =
    T.Toolkit (T.ToolkitName "raydraw") nodes
    where
        nodes NodeListNode = T.emptyNode
        nodes PaletteNode = paletteNode
        nodes BangNode = bangNode
        nodes DrawLogoNode = drawLogoNode
        nodes RayNode = rayNode
        nodes PreviewNode = T.emptyNode