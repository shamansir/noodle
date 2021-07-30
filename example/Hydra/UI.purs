module Hydra.UI where

import Prelude (($), const)

import Data.Maybe (Maybe(..))

import Hydra (Hydra)
import Hydra.Component.Background as BG
import Hydra.Component.Node.Num as NumNode

import Noodle.Node as Node

import App.UI as UI
import App.UI (UI)


ui :: UI Hydra
ui = { background, node }


background :: forall m. Maybe (UI.BgComponent m Hydra)
background = Just BG.component


node :: forall m. Node.Family -> Maybe (UI.NodeComponent m Hydra)
node "num" = Just $ NumNode.component
node _ = Nothing