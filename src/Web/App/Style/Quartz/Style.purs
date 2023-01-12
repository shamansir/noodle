module Web.App.Style.Quartz
    (style) where


import Data.Set as Set

import Web.App.Style
import Web.App.Style.Order as Order

import Web.App.Style.Quartz.Background (bg)
import Web.App.Style.Quartz.Body (body)
import Web.App.Style.Quartz.Title (title)
import Web.App.Style.Quartz.Slot (slot)
import Web.App.Style.Quartz.Link (link)
import Web.App.Style.Quartz.NodeTab (nodeTab)
import Web.App.Style.Quartz.PatchTab (patchTab)


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