module Toolkit.Hydra2.CliRender where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.SProxy (reflect)

import Cli.Keys (NodeBoxKey)
import Cli.Components.NodeBox.HasBody (class HasBody)
import Blessed.Internal.BlessedOp (BlessedOp)

import Noodle.Node2 (Node)
import Noodle.Node2 (family) as Node
import Noodle.Patch4 (HoldsNode', withNode')
import Noodle.Id (FamilyR)
import Noodle.Id as Id


import Toolkit.Hydra2 (State, Instances)

import Toolkit.Hydra2.Family.Feed.FNumber (Node) as FNumber
import Toolkit.Hydra2.Family.Render.Cli.Feed.FNumber (render) as FNumber
import Toolkit.Hydra2.Family.Render.Cli.Out.FOut (render) as FOut



-- render :: forall m. FamilyR -> Maybe (NodeBoxKey -> HoldsNode' State (Instances m) m -> BlessedOp State m)
-- render nbKey holdsNode familyR = case Id.reflectFamilyR familyR of
--         "number" -> withNode' holdsNode \_ node -> FNumber.render nbKey node
--         "out" -> withNode' holdsNode \_ node -> FOut.render nbKey node
--         _ -> Nothing


{-
render nbKey holdsNode familyR =
    withNode' holdsNode (\patch node ->
        case Id.reflectFamily' (Node.family node) of
            "number" -> FNumber.render nbKey node
            "out" -> FOut.render nbKey node
            _ -> pure unit
    )
-}