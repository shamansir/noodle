module App.Style.Quartz
    (style) where


import Data.Set as Set

import App.Style
import App.Style.Order as Order

import App.Style.Quartz.Background (bg)
import App.Style.Quartz.Body (body)
import App.Style.Quartz.Title (title)
import App.Style.Quartz.Slot (slot)
import App.Style.Quartz.Link (link)
import App.Style.Quartz.NodeTab (nodeTab)
import App.Style.Quartz.PatchTab (patchTab)


style :: Style
style =
    { order :
        Order.make
            [ Title, UserBodyBetweenSlots ]
    , bg
    , body
    , slot
    , link
    , title
    , supportedFlows : Set.singleton Vertical
    , font : { size : 7.0, family : [ "PT Mono", "Andale Mono", "Fira mono", "Menlo", "sans-serif" ] }
    , patchTab
    , nodeTab
    }