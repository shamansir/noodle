module Cli.Components.NodeBox.HasBody where

import Prelude
import Type.Proxy (Proxy)
import Data.Maybe (Maybe)
import Cli.Keys (NodeBoxKey)

import Signal (Signal)
import Blessed.Internal.BlessedOp (BlessedOp)
import Noodle.Node2 (Node)


class HasBody x f state is os m {- repr -} | x -> f, f -> state is os where
--     {-
--     component :: x -> Blessed state m
--     init :: x -> Blessed state m -> BlessedOp state m
--     -}
    run :: Proxy x -> NodeBoxKey -> Node f state is os m -> {- Signal repr -> -} BlessedOp state m


class HasBody' x y {- repr -} state m | x -> y state where
--     {-
--     component :: x -> Blessed state m
--     init :: x -> Blessed state m -> BlessedOp state m
--     -}
    run' :: Proxy x -> NodeBoxKey -> y -> {- Signal repr -> -} BlessedOp state m


-- type RenderBody f state is os m = NodeBoxKey -> Node f state is os m -> {- Signal repr -> -} BlessedOp state m