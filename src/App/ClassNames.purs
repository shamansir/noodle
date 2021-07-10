module App.ClassNames where


import Prelude ((<$>))
import Halogen as H


patchesTabs :: Array H.ClassName
patchesTabs = H.ClassName <$> [ "patches-tabs" ]


patchTab :: Array H.ClassName
patchTab = H.ClassName <$> [ "patch-tab", "no-select" ]


nodesTabs :: Array H.ClassName
nodesTabs = H.ClassName <$> [ "nodes-tabs" ]


nodeButton :: Array H.ClassName
nodeButton = H.ClassName <$> [ "node-button", "no-select" ]


nodeInlets :: Array H.ClassName
nodeInlets = H.ClassName <$> [ "node-inlets" ]


nodeOutlets :: Array H.ClassName
nodeOutlets = H.ClassName <$> [ "node-outlets" ]


nodes :: Array H.ClassName
nodes = H.ClassName <$> [ "nodes" ]
