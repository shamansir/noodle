module Cli.Class.CliRenderer where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Data.Tuple.Nested ((/\), type (/\))

import Type.Proxy (Proxy)
import Type.Data.Symbol (class IsSymbol)
import Data.Maybe (Maybe(..))

import Control.Monad.State (class MonadState)

import Blessed.Internal.BlessedOp (BlessedOp, BlessedOp')

import Noodle.Id (Family, FamilyR, NodeR, InletR) as Id
import Noodle.Node (Node)
import Noodle.Raw.Node (Node) as Raw
import Noodle.Toolkit (ToolkitKey)
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)
import Noodle.Repr.ValueInChannel (ValueInChannel)

import Cli.Keys (NodeBoxKey)
import Cli.Components.ValueEditor (ValueEditor)


class CliLocator :: Type -> Constraint
class CliLocator x where
    firstLocation :: x
    locateNext :: x -> { width :: Int, height :: Int } -> x /\ { left :: Int, top :: Int }
    -- defaultSize :: Proxy x -> { width :: Int, height :: Int }


data ConstantShift
    = First
    | NextAfter { left :: Int, top :: Int }


instance CliLocator ConstantShift where
    firstLocation :: ConstantShift
    firstLocation = First
    locateNext :: ConstantShift -> { width :: Int, height :: Int } -> ConstantShift /\ { left :: Int, top :: Int }
    locateNext (NextAfter { left, top }) _ =
        let
            nextPos =
                { left : 16 + left + 2
                , top : top + 2
                }
        in NextAfter nextPos /\ nextPos
    locateNext First _ = NextAfter { left : 16, top : 0 } /\ { left : 16, top : 0 }
    -- defaultSize :: Proxy PixelShift -> { width :: Int, height :: Int }
    -- defaultSize _ = { width : 5, height : 2 }


class CliRenderer (tk :: ToolkitKey) (fs :: Families) repr m | tk -> fs where
    cliSize :: forall (f :: Symbol) fstate is os. RegisteredFamily (F f fstate is os repr m) fs => Proxy tk -> Proxy fs -> Id.Family f -> NodeBoxKey -> Node f fstate is os repr m -> Maybe { width :: Int, height :: Int }
    renderCli :: forall (f :: Symbol) fstate is os. MonadEffect m => IsSymbol f => RegisteredFamily (F f fstate is os repr m) fs => Proxy tk -> Proxy fs -> Id.Family f -> NodeBoxKey -> Node f fstate is os repr m -> Maybe (BlessedOp fstate m)


class CliRawRenderer (tk :: ToolkitKey) (fs :: Families) repr m | tk -> fs where
    cliSizeRaw :: forall fstate. Proxy tk -> Proxy fs -> Id.FamilyR -> NodeBoxKey -> Raw.Node fstate repr m -> Maybe { width :: Int, height :: Int }
    renderCliRaw :: forall fstate. Proxy tk -> Proxy fs -> Id.FamilyR -> NodeBoxKey -> Raw.Node fstate repr m -> Maybe (BlessedOp fstate m)


class CliEditor (tk :: ToolkitKey) repr | tk -> repr where
    editorFor :: Proxy tk -> Id.FamilyR -> NodeBoxKey -> Id.NodeR {- Raw.Node fstate repr m -} -> Id.InletR -> ValueInChannel repr -> Maybe (ValueEditor repr Unit Effect)
