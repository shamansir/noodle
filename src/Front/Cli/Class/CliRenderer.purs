module Cli.Class.CliRenderer where

import Prelude


import Type.Proxy (Proxy)
import Data.Maybe (Maybe)
import Cli.Keys (NodeBoxKey)

import Blessed.Internal.BlessedOp (BlessedOp)
import Noodle.Id (Family, FamilyR) as Id
import Noodle.Node (Node)
import Noodle.Raw.Node (Node) as Raw
import Noodle.Toolkit (ToolkitKey)
import Noodle.Toolkit.Families (Families, F, class RegisteredFamily)


class CliRenderer (tk :: ToolkitKey) (fs :: Families) repr m | tk -> fs where
    cliSize :: forall (f :: Symbol) fstate is os. RegisteredFamily (F f fstate is os repr m) fs => Proxy tk -> Proxy fs -> Id.Family f -> NodeBoxKey -> Node f fstate is os repr m -> Maybe { width :: Int, height :: Int }
    cliSizeRaw :: forall fstate. Proxy tk -> Proxy fs -> Id.FamilyR -> NodeBoxKey -> Raw.Node fstate repr m -> Maybe { width :: Int, height :: Int }
    renderCli :: forall (f :: Symbol) fstate is os. RegisteredFamily (F f fstate is os repr m) fs => Proxy tk -> Proxy fs -> Id.Family f -> NodeBoxKey -> Node f fstate is os repr m -> BlessedOp fstate m
    renderCliRaw :: forall fstate. Proxy tk -> Proxy fs -> Id.FamilyR -> NodeBoxKey -> Raw.Node fstate repr m -> BlessedOp fstate m
