module Toolkit.Hydra2.Family.Render.Cli.Feed.FNumber where

import Prelude

import Type.Proxy (Proxy)
import Cli.Components.NodeBox.HasBody (class HasBody)

import Cli.Keys (NodeBoxKey)

import Data.Maybe (Maybe(..))

import Signal (Signal)
import Blessed.Internal.BlessedOp (BlessedOp)

-- import Noodle.Node2 (Node)

import Toolkit.Hydra2.Family.Feed.FNumber (Inputs, Outputs, State, Node)


-- instance HasBody "number" State Inputs Outputs m where
--     run :: NodeBoxKey -> Node m -> Maybe (BlessedOp State m)
--     run _ _ = Nothing


render :: forall m. NodeBoxKey -> Node m -> BlessedOp State m
render _ _ = pure unit




-- render :: forall m. RenderBody "number" State Inputs Outputs m
-- render nbKey node = pure unit