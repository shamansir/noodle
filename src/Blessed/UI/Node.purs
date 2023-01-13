module Blessed.UI.Node where

import Prelude


import Blessed.UI.Screen as Screen
import Blessed.UI.Box as Box


data Options
    = Box Box.Options
    | Screen Screen.Options
    | Image


newtype Node =
    Node
        { options :: Options
        -- , parent :: Maybe NodeId
        , children :: Array Node
        }
