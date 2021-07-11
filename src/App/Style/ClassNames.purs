module App.Style.ClassNames where


import Prelude ((<$>), (<>))
import Halogen as H


import App.Style (NodeFlow(..))



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


nodeButton :: String -> Array H.ClassName
nodeButton name = H.ClassName <$> [ "node-button", "node-tab-" <> name, "no-select" ]


node :: NodeFlow -> String -> Array H.ClassName
node flow name = H.ClassName <$> [ "node", "node-" <> flowStr flow, "node-" <> name, "no-select" ]
    where
    flowStr Vertical = "vert"
    flowStr Horizontal = "horz"


nodeInlets :: Array H.ClassName
nodeInlets = H.ClassName <$> [ "node-inlets" ]


inlet :: String -> Array H.ClassName
inlet name = H.ClassName <$> [ "inlet", "inlet-" <> name ]


nodeOutlets :: Array H.ClassName
nodeOutlets = H.ClassName <$> [ "node-outlets" ]


outlet :: String -> Array H.ClassName
outlet name = H.ClassName <$> [ "outlet", "outlet-" <> name ]


nodes :: Array H.ClassName
nodes = H.ClassName <$> [ "nodes" ]


slotFocusArea :: Array H.ClassName
slotFocusArea = H.ClassName <$> [ "slot-focus" ]