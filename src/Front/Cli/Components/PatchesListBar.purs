module Cli.Components.PatchesListbar where

import Prelude

import Control.Monad.State as State

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (toUnfoldable) as Map
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\), type (/\))

import Blessed as B
import Blessed ((>~))

import Blessed.Core.Key (Key)
import Blessed.Internal.Core as Core
import Blessed.Internal.BlessedOp as Core
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

import Cli.Keys as Key
import Cli.State (State)
import Cli.Style as Style


component :: forall tk s fs sr cr m. Map Id.PatchR (Patch s fs sr cr m) -> Core.Blessed (State tk s fs sr cr m)
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

type Command_ subj id tk s fs sr cr m =
    (  String
    /\ Array Key
    /\ Core.HandlerFn subj id (State tk s fs sr cr m)
    )


lbCommands
    :: forall tk s fs sr cr m
    .  Map Id.PatchR (Patch s fs sr cr m)
    -> Array (Command_ _ _ tk s fs sr cr m)
lbCommands = mapWithIndex buttonFor <<< Map.toUnfoldable


buttonFor
    :: forall tk s fs sr cr m
    .  Int
    -> Id.PatchR /\ Patch s fs sr cr m
    -> Command_ _ _ tk s fs sr cr m
buttonFor index (id /\ patch) =
    Patch.name patch /\ [] /\ \_ _ -> do
        State.modify_
            (_ { currentPatch = Just { index, id } })
        -- patchesBar >~ ListBar.selectTab index
        Key.mainScreen >~ Screen.render


updatePatches :: forall tk s fs sr cr mi mo. Map Id.PatchR (Patch s fs sr cr mi) -> Core.BlessedOp (State tk s fs sr cr mi) mo
updatePatches patches =
    Key.patchesBar >~ ListBar.setItems $ lbCommands patches


selectPatch :: forall tk s fs sr cr mi mo. Int -> Core.BlessedOp (State tk s fs sr cr mi) mo
selectPatch patchNumId =
    Key.patchesBar >~ ListBar.select patchNumId