module Cli.Components.NodeBox.HasBody where

import Type.Proxy (Proxy)
import Data.Maybe (Maybe)
import Cli.Keys (NodeBoxKey)

import Blessed.Internal.BlessedOp (BlessedOp)
import Noodle.Id (Family, FamilyR) as Id
import Noodle.Node (Node)
import Noodle.Raw.Node (Node) as Raw
import Noodle.Toolkit (Toolkit, ToolkitKey)
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)


-- import Toolkit.Hydra.Family.Render.RenderTarget

-- TODO: we have the conflict between m for `Node` and `BlessedOp` in the result,
--       we can not afford `MonadEffect` for `BlessedOp` due to problems in compiling handlers (see `HandlerFn` in `Blessed` which is bound to `Effect` for this reason)

{-
class HasBody :: forall k. k -> Symbol -> Type -> Row Type -> Row Type -> (Type -> Type) -> Constraint
class HasBody x f state is os m repr | x -> f, f -> state is os where
--     component :: x -> Blessed state m
--     init :: x -> Blessed state m -> BlessedOp state m
    run :: Proxy x -> NodeBoxKey -> Node f state is os m -> Signal repr -> BlessedOp state m
-}

-- TODO: better lock the typeclass on `F` family instance? Because we use it only for nodes? Aren't we?
--       see `RegisteredFamily`
--class HasCliBody :: forall k. k -> Type -> Type -> (Type -> Type) -> Constraint
{- REM
class HasCliBody (tk :: ToolkitKey) (f :: Symbol) x state m | tk f -> x state m where
    runBlessed :: Proxy tk -> Proxy f -> NodeBoxKey -> x -> BlessedOp state m
-}


{-
type Renderer f nstate is os repr m =
    (  NodeBoxKey
    -> Node f nstate is os repr m
    ->
        { size :: Maybe { width :: Int, height :: Int }
        , node :: BlessedOp nstate m
        }
    )


type RawRenderer repr m =
    (  NodeBoxKey
    -> Raw.Node repr m
    ->
        { size :: Maybe { width :: Int, height :: Int }
        , node :: BlessedOp repr m
        }
    )
-}


class CliRenderer (tk :: ToolkitKey) (fs :: Families) repr m | tk -> fs where
    cliSize :: forall (f :: Symbol) nstate is os. RegisteredFamily (F f nstate is os repr m) fs => Proxy tk -> Proxy fs -> Id.Family f -> NodeBoxKey -> Node f nstate is os repr m -> Maybe { width :: Int, height :: Int }
    cliSizeRaw :: Proxy tk -> Proxy fs -> Id.FamilyR -> NodeBoxKey -> Raw.Node repr m -> Maybe { width :: Int, height :: Int }
    renderCli :: forall (f :: Symbol) nstate is os. RegisteredFamily (F f nstate is os repr m) fs => Proxy tk -> Proxy fs -> Id.Family f -> NodeBoxKey -> Node f nstate is os repr m -> BlessedOp repr m
    renderCliRaw :: Proxy tk -> Proxy fs -> Id.FamilyR -> NodeBoxKey -> Raw.Node repr m -> BlessedOp repr m


{-
class HasBody'' :: RenderItem -> Symbol -> Type -> Row Type -> Row Type -> (Type -> Type) -> Constraint
class HasBody'' x f state is os m repr | x -> f, f -> state is os where
--     component :: x -> Blessed state m
--     init :: x -> Blessed state m -> BlessedOp state m
    run'' :: Proxy x -> NodeBoxKey -> Node f state is os m -> Signal repr -> BlessedOp state m
-}


{-
class HasBody''' :: RenderItem -> Type -> Type -> (Type -> Type) -> Constraint
class HasBody''' x y repr state m | x -> y state where
--     component :: x -> Blessed state m
--     init :: x -> Blessed state m -> BlessedOp state m
    run''' :: Proxy x -> NodeBoxKey -> y -> Signal repr -> BlessedOp state m
-}


-- TODO: better lock the typeclass on `F` family instance? Because we use it only for nodes? Aren't we?
--       see `RegisteredFamily`

{- REM
class HasCliCustomSize (tk :: ToolkitKey) (f :: Symbol) x | tk f -> x where
    cliSize :: Proxy tk -> Proxy f -> NodeBoxKey -> x -> Maybe { width :: Int, height :: Int }
-}


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