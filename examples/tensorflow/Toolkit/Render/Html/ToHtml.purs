module TensorFlow.Toolkit.Render.Html.ToHtml where


import Prelude (($), (<<<), show)

import Spork.Html as H

import Noodle.Path as P
import Noodle.API.Action (Action(..), RequestAction(..)) as A

import Noodle.Render.Html (View, RoutedAction, core) as R

import TensorFlow.Toolkit.Node (Node)
import TensorFlow.Toolkit.Value (Value)
import TensorFlow.Toolkit.Channel (Channel)


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
