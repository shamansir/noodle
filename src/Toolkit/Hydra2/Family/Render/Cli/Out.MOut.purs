module Toolkit.Hydra2.Family.Render.Cli.Out.FOut where

import Prelude

import Type.Proxy (Proxy)
import Cli.Components.NodeBox.HasBody (class HasBody)

import Cli.Keys (NodeBoxKey)

import Data.Maybe (Maybe(..))

import Signal (Signal)
import Blessed.Internal.BlessedOp (BlessedOp)

-- import Noodle.Node2 (Node)

import Toolkit.Hydra2.Family.Out.FOut (Inputs, Outputs, State, Node)


render :: forall m. NodeBoxKey -> Node m -> BlessedOp State m
render _ _ = pure unit