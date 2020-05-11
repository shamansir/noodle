module Xodus.Toolkit.Render.Html.ToHtml where


import Prelude (($), (<<<), show)

import Spork.Html as H

import Noodle.Path as P
import Noodle.API.Action (Action(..), RequestAction(..)) as A

import Noodle.Render.Html (View, RoutedAction, core) as R

import Xodus.Toolkit.Node (Node)
import Xodus.Toolkit.Value (Value)
import Xodus.Toolkit.Channel (Channel)


type View = R.View Value Channel Node


type Action = R.RoutedAction Value Channel Node


class ToHtml a where
    toHtml :: P.ToNode -> a -> View


instance toHtmlString :: ToHtml String where
    toHtml _ = H.text


instance toHtmlInt :: ToHtml Int where
    toHtml _ = H.text <<< show


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
