module Hydra.UI where

import Prelude (($), const)

import Data.Maybe (Maybe(..))

import Hydra (Hydra)
import Hydra.Component.Background as BG
import Hydra.Component.Node.Num as NumNode

import App.UI as UI
import App.UI (UI)


ui :: UI Hydra
ui = { background, node }


background :: forall m. Maybe (UI.BgComponent m Hydra)
background = Just BG.component


node :: forall m. String -> Maybe (UI.NodeComponent m Hydra)
node = const $ Just $ NumNode.component