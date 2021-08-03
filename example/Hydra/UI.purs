module Hydra.UI where

import Prelude (($), const)

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)

import Hydra (Hydra)
import Hydra.Component.Background as BG
import Hydra.Component.Node.Num as NumNode
import Hydra.Component.Node.Osc as OscNode

import Noodle.Node as Node

import App.UI as UI
import App.UI (UI)


ui :: forall m. MonadEffect m => UI m Hydra
ui = { background, node }


background :: forall m. Maybe (UI.BgComponent m Hydra)
background = Just BG.component


node :: forall m. MonadEffect m => Node.Family -> Maybe (UI.NodeComponent m Hydra)
node "num" = Just $ NumNode.component
node "osc" = Just $ OscNode.component
node _ = Nothing