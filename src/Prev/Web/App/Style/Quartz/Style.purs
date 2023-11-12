module Prev.Web.App.Style.Quartz
    (style) where


import Data.Set as Set

import Prev.Web.App.Style
import Prev.Web.App.Style.Order as Order

import Prev.Web.App.Style.Quartz.Background (bg)
import Prev.Web.App.Style.Quartz.Body (body)
import Prev.Web.App.Style.Quartz.Title (title)
import Prev.Web.App.Style.Quartz.Slot (slot)
import Prev.Web.App.Style.Quartz.Link (link)
import Prev.Web.App.Style.Quartz.NodeTab (nodeTab)
import Prev.Web.App.Style.Quartz.PatchTab (patchTab)


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