module Toolkit.Hydra2.Family.Render.Cli where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Type.Proxy (Proxy)
import Cli.Components.NodeBox.HasBody (class HasBody, class HasBody')

import Cli.Keys (NodeBoxKey)

import Blessed.Internal.BlessedOp (BlessedOp)

import Toolkit.Hydra2.Family.Feed.FNumber (Inputs, Outputs, State, Node) as FNumber
import Toolkit.Hydra2.Family.Render.Cli.Feed.FNumber (render) as FNumber
import Toolkit.Hydra2.Family.Out.FOut (Inputs, Outputs, State, Node) as FOut
import Toolkit.Hydra2.Family.Render.Cli.Out.FOut (render) as FOut

import Noodle.Node2 (Node)


data Cli (f :: Symbol) = Cli


instance (Applicative m, MonadEffect m) => HasBody (Cli "number") "number" FNumber.State FNumber.Inputs FNumber.Outputs m where
    run :: Proxy (Cli "number") -> NodeBoxKey -> FNumber.Node m -> BlessedOp FNumber.State m
    run _ = FNumber.render
else instance HasBody (Cli "out") "out" FOut.State FOut.Inputs FOut.Outputs m where
    run :: Proxy (Cli "out") -> NodeBoxKey -> FOut.Node m -> BlessedOp FOut.State m
    run _ = FOut.render
else instance HasBody (Cli f) f state is os m where
    run :: Proxy (Cli f) -> NodeBoxKey -> Node f state is os m -> BlessedOp state m
    run _ _ _ = pure unit


instance (Applicative m, MonadEffect m) => HasBody' (Cli "number") (FNumber.Node m) FNumber.State m where
   run' :: Proxy (Cli "number") -> NodeBoxKey -> FNumber.Node m -> BlessedOp FNumber.State m
   run' _ = FNumber.render
else instance HasBody' (Cli "out") (FOut.Node m) FOut.State m where
   run' :: Proxy (Cli "out") -> NodeBoxKey -> FOut.Node m -> BlessedOp FOut.State m
   run' _ = FOut.render
else instance HasBody' (Cli f) (Node f state is os m) state m where
   run' :: Proxy (Cli f) -> NodeBoxKey -> Node f state is os m -> BlessedOp state m
   run' _ _ _ = pure unit