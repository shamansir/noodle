module RayDraw.Toolkit.Render.Html.ToHtml where


import Prelude (($), (<<<), show)

import Spork.Html as H

import Noodle.Path as P
import Noodle.API.Action (Action(..), RequestAction(..)) as A

import Noodle.Render.Html (View, RoutedAction, ToolkitRenderer, core) as R


import RayDraw.Toolkit.Node (Node)
import RayDraw.Toolkit.Value (Value)
import RayDraw.Toolkit.Channel (Channel)


type Action = R.RoutedAction Value Channel Node

type View = R.View Value Channel Node


toInlet :: P.ToNode -> P.Alias -> Value -> Action
toInlet path alias v =
    R.core
        $ A.Request
        $ A.ToSendToInlet (P.inletInNode path alias)
        $ v


toOutlet :: P.ToNode -> P.Alias -> Value -> Action
toOutlet path alias v =
    R.core
        $ A.Request
        $ A.ToSendToOutlet (P.outletInNode path alias)
        $ v