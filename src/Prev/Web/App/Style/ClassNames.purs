module Prev.Web.App.Style.ClassNames where


import Prelude ((<$>), (<>), show)
import Halogen as H


import Web.App.Style (NodeFlow(..), SlotDirection(..))



patchesTabs :: Array H.ClassName
patchesTabs = H.ClassName <$> [ "patches-tabs" ]


patchTab :: String -> Array H.ClassName
patchTab name = H.ClassName <$> [ "patch-tab", "patch-tab-" <> name, "no-select" ]


nodesTabs :: Array H.ClassName
nodesTabs = H.ClassName <$> [ "nodes-tabs" ]


nodeTitle :: Array H.ClassName
nodeTitle = H.ClassName <$> [ "node-title" ]


nodeTitleFocus :: Array H.ClassName
nodeTitleFocus = H.ClassName <$> [ "node-title-focus" ]


nodeTitleFocusDebug :: Array H.ClassName
nodeTitleFocusDebug = H.ClassName <$> [ "node-title-focus", "debug" ]


nodeButton :: String -> Array H.ClassName
nodeButton name = H.ClassName <$> [ "node-button", "node-tab-" <> name, "no-select" ]


removeButton :: Array H.ClassName
removeButton = H.ClassName <$> [ "remove-button" ]


node :: NodeFlow -> String -> Array H.ClassName
node flow name = H.ClassName <$> [ "node", "node-" <> flowStr flow, "node-" <> name, "no-select" ]
    where
    flowStr Vertical = "vert"
    flowStr Horizontal = "horz"


nodeInlets :: SlotDirection -> Array H.ClassName
nodeInlets dir = H.ClassName <$> [ "node-inlets", "dir-" <> show dir ]


inlet :: String -> Array H.ClassName
inlet name = H.ClassName <$> [ "inlet", "inlet-" <> name ]


nodeOutlets :: SlotDirection -> Array H.ClassName
nodeOutlets dir = H.ClassName <$> [ "node-outlets", "dir-" <> show dir ]


outlet :: String -> Array H.ClassName
outlet name = H.ClassName <$> [ "outlet", "outlet-" <> name ]


nodes :: Array H.ClassName
nodes = H.ClassName <$> [ "nodes" ]


slotIdLabel :: Array H.ClassName
slotIdLabel = H.ClassName <$> [ "slot-id-label" ]


slotFocusArea :: Array H.ClassName
slotFocusArea = H.ClassName <$> [ "slot-focus" ]


slotFocusAreaDebug :: Array H.ClassName
slotFocusAreaDebug = H.ClassName <$> [ "slot-focus", "debug" ]