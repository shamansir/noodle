module Hydra.UI where

import Prelude (($), const)

import Data.Maybe (Maybe(..))

import Hydra (Hydra)
import Hydra.Component.Background as BG

import Halogen as H

import App.UI (UI, BgInput, NodeInput)


ui :: UI Hydra
ui = { background, node }


background :: forall m query output. Maybe (H.Component query (BgInput Hydra) output m)
background = Just BG.component


node :: forall m query output. String -> Maybe (H.Component query (NodeInput Hydra) output m)
node = const $ Nothing