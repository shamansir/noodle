module Hydra.Toolkit.UI.Components where


import App.Toolkit.UI as UI

import Hydra (Hydra)
import Hydra.Toolkit.UI.Action (Action) as UI
import Hydra.Toolkit.UI.State (State) as UI

import Halogen (HalogenM) as H


type PatchComponent m = UI.PatchComponent' UI.Action UI.State Hydra m


type NodeComponent m = UI.NodeComponent' UI.Action UI.State Hydra m


type BgComponent m = UI.BgComponent' UI.State Hydra m


type Components m = UI.Components' UI.Action UI.State Hydra m


type NodeInput = UI.NodeInput' UI.State Hydra


type NodeOutput = UI.NodeOutput' UI.Action Hydra


type PatchInput = UI.PatchInput' UI.State Hydra


--type PatchOutput = UI.PatchOutput' UI.Action Hydra


type BgInput = UI.BgInput' UI.State Hydra


--type BgOutput = UI.BgOutput' UI.State Hydra