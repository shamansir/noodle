module Cli.Components.NodeBox.HasBody where

import Prelude
import Type.Proxy (Proxy)
import Data.Maybe (Maybe)
import Cli.Keys (NodeBoxKey)

import Signal (Signal)
import Blessed.Internal.BlessedOp (BlessedOp)
import Noodle.Node (Node)
import Noodle.Id (Input, class HasInput)


-- TODO: we have the conflict between m for `Node` and `BlessedOp` in the result,
--       we can not afford `MonadEffect` for `BlessedOp` due to problems in compiling handlers (see `HandlerFn` in `Blessed` which is bound to `Effect` for this reason)

class HasBody :: forall k. k -> Symbol -> Type -> Row Type -> Row Type -> (Type -> Type) -> Constraint
class HasBody x f state is os m {- repr -} | x -> f, f -> state is os where
--     {-
--     component :: x -> Blessed state m
--     init :: x -> Blessed state m -> BlessedOp state m
--     -}
    run :: Proxy x -> NodeBoxKey -> Node f state is os m -> {- Signal repr -> -} BlessedOp state m


class HasBody' :: forall k. k -> Type -> Type -> (Type -> Type) -> Constraint
class HasBody' x y {- repr -} state m | x -> y state where
--     {-
--     component :: x -> Blessed state m
--     init :: x -> Blessed state m -> BlessedOp state m
--     -}
    run' :: Proxy x -> NodeBoxKey -> y -> {- Signal repr -> -} BlessedOp state m


class HasCustomSize :: forall k. k -> Type -> Constraint
class HasCustomSize x y | x -> y where
    size :: Proxy x -> NodeBoxKey -> y -> Maybe { width :: Int, height :: Int }


{-
-- class HasEditor :: forall k. k -> Type -> Type -> Type -> (Type -> Type) -> Constraint
class HasEditor x is y z state m | x -> state, z -> y state is where
    editor :: Proxy is -> Proxy x -> NodeBoxKey -> y -> z -> Maybe (BlessedOp state m)


class HasInput i din is' is <= HasEditor' x y i is' is din m | x -> y, y -> is m, is -> is' where
    editor' :: Proxy x -> Proxy is -> Proxy is' -> NodeBoxKey -> Input i -> y -> Maybe (BlessedOp din m)


class HasEditor'' x i din f state is' is os m | x -> f, f -> state is, is -> is' where
    editor'' :: Proxy din -> Proxy is -> Proxy x -> NodeBoxKey -> Input i -> Node f state is os m -> Maybe (BlessedOp din m)
-}



-- type RenderBody f state is os m = NodeBoxKey -> Node f state is os m -> {- Signal repr -> -} BlessedOp state m


-- newtype HoldsNode = HoldsNode (forall r. (forall f state is os m. IsSymbol f => Node f state is os m -> r) -> r)