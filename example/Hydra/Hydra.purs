module Hydra where


import Prelude (($), const)

import Data.Maybe (Maybe(..))

import App.UI (class UI, BgInput, NodeInput)

import Halogen as H


data Hydra = Hydra


default :: Hydra
default = Hydra


instance hydraUI :: UI Hydra where
    background :: forall m query output. Maybe (H.Component query (BgInput Hydra) output m)
    background = Nothing
    node :: forall m query output. String -> Maybe (H.Component query (NodeInput Hydra) output m)
    node = const $ Nothing