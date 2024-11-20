module Cli.Components.PatchesListbar where

import Prelude

import Control.Monad.State as State

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (toUnfoldable) as Map
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\))

import Blessed as B
import Blessed ((>~))

import Blessed.Internal.Core as Core
import Blessed.Core.Offset as Offset
import Blessed.Core.Dimension as Dimension
import Blessed.Core.ListStyle as LStyle
import Blessed.Core.EndStyle as ES

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Lists.List.Option as List
import Blessed.UI.Lists.ListBar.Option (commands) as ListBar
import Blessed.UI.Lists.ListBar.Method (setItems, select) as ListBar
import Blessed.UI.Base.Screen.Method as Screen

import Noodle.Patch (Patch)
import Noodle.Patch (name) as Patch
import Noodle.Id (PatchR) as Id
import Noodle.Ui.Cli.Palette as Palette

import Cli.Keys as Key
import Cli.State (State)
import Cli.Style as Style


component :: forall s fs repr m. Map Id.PatchR (Patch s fs repr m) -> Core.Blessed (State s fs repr m)

-- Map Patch.Id (Patch gstate instances)
-- component ∷ ∀ (t106 ∷ Type) (t110 ∷ Row Type). Map String t106 → SNode { currentPatch ∷ Maybe (Tuple Int String) | t110 }
component patches =
    B.listbar Key.patchesBar
        [ Box.top $ Offset.px 0
        , Box.left $ Offset.px 0
        , Box.width $ Dimension.percents 100.0
        , Box.height $ Dimension.px 1
        , List.mouse true
        -- , List.items patches
        , ListBar.commands $ lbCommands patches
        , Style.patchesBar
        ]
        []


--patchesLBCommands ∷ ∀ (t41 ∷ Type -> Type) (t45 ∷ Type) (t46 ∷ Type) (t47 ∷ Type) (t48 ∷ Type) (t49 ∷ Row Type) (t50 ∷ Type) (t51 ∷ Type) (t52 ∷ Type -> Type). FunctorWithIndex t50 t41 ⇒ Unfoldable t41 ⇒ Map t51 t45 → t41 (Tuple t51 (Tuple (Array t46) (t47 → t48 → BlessedOpM { currentPatch ∷ Maybe (Tuple t50 t51) | t49 } t52 Unit ) ) )
lbCommands = mapWithIndex buttonFor <<< Map.toUnfoldable


--patchButton ∷ ∀ (t5 ∷ Type) (t10 ∷ Type) (t11 ∷ Type) (t13 ∷ Type) (t20 ∷ Row Type) (t24 ∷ Type) (t25 ∷ Type) (t30 ∷ Type -> Type). t24 → Tuple t25 t5 → Tuple t25 (Tuple (Array t10) (t11 → t13 → BlessedOpM { currentPatch ∷ Maybe (Tuple t24 t25) | t20 } t30 Unit ) )
buttonFor index (id /\ patch) =
    Patch.name patch /\ [] /\ \_ _ -> do
        State.modify_
            (_ { currentPatch = Just { index, id } })
        -- patchesBar >~ ListBar.selectTab index
        Key.mainScreen >~ Screen.render


updatePatches patches =
    Key.patchesBar >~ ListBar.setItems $ lbCommands patches


selectPatch patchNumId =
    Key.patchesBar >~ ListBar.select patchNumId