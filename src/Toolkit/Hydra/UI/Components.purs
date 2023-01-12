module Toolkit.Hydra.UI.Components where

import Prelude (Unit)

import Web.App.Toolkit.UI as UI

import Toolkit.Hydra.Op (Hydra)
import Toolkit.Hydra.UI.Action (Action) as UI
import Toolkit.Hydra.UI.State (State) as UI

import Halogen (HalogenM) as H


type PatchComponent = UI.PatchComponent' UI.Action UI.State Hydra -- FIXME: no `m` here, `... Unit Hydra`


type NodeComponent = UI.NodeComponent' UI.Action UI.State Unit Hydra


type Components = UI.Components' UI.Action UI.State Unit Hydra


type NodeInput = UI.NodeInput' UI.State Unit Hydra
type NodeOutput = UI.NodeOutput' UI.Action Hydra


type PatchInput = UI.PatchInput' UI.State Hydra
type PatchOutput = UI.PatchOutput' UI.State
type PatchQuery a = UI.PatchQuery' UI.Action a


--type PatchOutput = UI.PatchOutput' UI.Action Hydra


-- type BgInput = UI.BgInput' UI.State Hydra


--type BgOutput = UI.BgOutput' UI.State Hydra