module Toolkit.Hydra2.Family.Render.Cli where

import Prelude

import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))
import Cli.Components.NodeBox.HasBody (class HasBody, run, class HasBody', run')

import Cli.Keys (NodeBoxKey)

import Signal (Signal)
import Blessed.Internal.BlessedOp (BlessedOp)

import Toolkit.Hydra2.Family.Feed.FNumber (Inputs, Outputs, State, Node) as FNumber
import Toolkit.Hydra2.Family.Render.Cli.Feed.FNumber (render) as FNumber
import Toolkit.Hydra2.Family.Out.FOut (Inputs, Outputs, State, Node) as FOut
import Toolkit.Hydra2.Family.Render.Cli.Out.FOut (render) as FOut

import Noodle.Node2 (Node)


data Cli (f :: Symbol) = Cli


instance HasBody (Cli "number") "number" FNumber.State FNumber.Inputs FNumber.Outputs m where
   run :: Proxy (Cli "number") -> NodeBoxKey -> FNumber.Node m -> BlessedOp FNumber.State m
   run _ = FNumber.render


instance HasBody' (Cli "number") (FNumber.Node m) FNumber.State m where
   run' :: Proxy (Cli "number") -> NodeBoxKey -> FNumber.Node m -> BlessedOp FNumber.State m
   run' _ = FNumber.render


instance HasBody (Cli "out") "out" FOut.State FOut.Inputs FOut.Outputs m where
   run :: Proxy (Cli "out") -> NodeBoxKey -> FOut.Node m -> BlessedOp FOut.State m
   run _ = FOut.render


instance HasBody' (Cli "out") (FOut.Node m) FOut.State m where
   run' :: Proxy (Cli "out") -> NodeBoxKey -> FOut.Node m -> BlessedOp FOut.State m
   run' _ = FOut.render


testFn ::
    forall f state is os m.
    HasBody (Cli f) f state is os m
    => NodeBoxKey -> Node f state is os m -> BlessedOp state m
testFn = run (Proxy :: _ (Cli f))


testFn2 ::
    forall f state is os m.
    HasBody' (Cli f) (Node f state is os m) state m
    => NodeBoxKey -> Node f state is os m -> BlessedOp state m
testFn2 = run' (Proxy :: _ (Cli f))
