module Web.App.Style.Hydra
    (style) where


import Data.Set as Set

import Web.App.Style
import Web.App.Style.Order as Order

import Web.App.Style.Hydra.Background (bg)
import Web.App.Style.Hydra.Body (body)
import Web.App.Style.Hydra.Title (title)
import Web.App.Style.Hydra.Slot (slot)
import Web.App.Style.Hydra.Link (link)
import Web.App.Style.Hydra.NodeTab (nodeTab)
import Web.App.Style.Hydra.PatchTab (patchTab)


style :: Style
style =
    { order :
        Order.make
            [ Title, Ribbon, UserBody 55.0, OnlyInlets, OnlyOutlets ]
    , bg
    , body
    , slot
    , link
    , title
    , supportedFlows : Set.singleton Vertical
    , font : { size : 7.0, family : [ "Trispace", "PT Mono", "Andale Mono", "Fira mono", "Menlo" ] }
    , patchTab
    , nodeTab
    }
