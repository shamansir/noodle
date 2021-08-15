module App.Style.Hydra
    (style) where


import Data.Set as Set

import App.Style
import App.Style.Order as Order

import App.Style.Hydra.Background (bg)
import App.Style.Hydra.Body (body)
import App.Style.Hydra.Title (title)
import App.Style.Hydra.Slot (slot)
import App.Style.Hydra.Link (link)
import App.Style.Hydra.NodeTab (nodeTab)
import App.Style.Hydra.PatchTab (patchTab)


style :: Style
style =
    { order :
        Order.make
            [ Title, UserBody 55.0, OnlyInlets, OnlyOutlets ]
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
