module Hydra.Toolkit.UI.Components where


import App.Toolkit.UI (BgComponent', Components', NodeComponent', PatchComponent') as UI

import Hydra (Hydra)
import Hydra.Toolkit.UI.Action (Action) as UI
import Hydra.Toolkit.UI.State (State) as UI


type PatchComponent m = UI.PatchComponent' UI.Action UI.State Hydra m


type NodeComponent m = UI.NodeComponent' UI.Action UI.State Hydra m


type BgComponent m = UI.BgComponent' UI.State Hydra m


type Components m = UI.Components' UI.Action UI.State Hydra m