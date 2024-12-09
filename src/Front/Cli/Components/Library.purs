module Cli.Components.Library where


import Control.Monad.State (get, modify_) as State

import Type.Proxy (Proxy(..))
import Type.Data.Symbol (class IsSymbol)

import Effect (Effect)

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Array ((!!))

import Data.Text.Output.Blessed (singleLine) as T


import Blessed as B
import Blessed ((>~), (~<))

import Blessed.Core.Offset as Offset
import Blessed.Core.Dimension as Dimension

import Blessed.Internal.Core as Core
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedOp (lift') as Blessed

import Blessed.UI.Boxes.Box.Option as Box
import Blessed.UI.Lists.List.Event (ListEvent(..)) as List
import Blessed.UI.Lists.List.Option (items, keys, mouse) as List
import Blessed.UI.Lists.List.Property (selected) as List

import Cli.Keys as Key
import Cli.State (State)
import Cli.State (withCurrentPatch) as State
import Cli.Style (library, libraryBorder) as Style

import Noodle.Id (PatchR, FamilyR, Family) as Id
import Noodle.Repr (class HasFallback, class FromToRepr)
import Noodle.Network (toolkit) as Network
import Noodle.Toolkit (class HoldsFamilies, families, spawn, spawnAnyRaw, withAnyFamily) as Toolkit
import Noodle.Toolkit.Family (Family) as Toolkit
import Noodle.Toolkit.Families (F, class RegisteredFamily)
import Noodle.Toolkit (Toolkit)
import Noodle.Ui.Cli.Tagging (libraryItem) as T
import Noodle.Wiring (class Wiring)
import Noodle.Fn.ToFn (class PossiblyToFn)
import Noodle.Node (Node) as Noodle
import Noodle.Patch (registerRawNode) as Patch
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Toolkit.Family (Family) as Raw

import Cli.Components.NodeBox as NodeBox
import Cli.Class.CliFriendly (class CliFriendly)


import Prelude


component
    :: forall tk p fs repr
     . FromToRepr repr repr
    => Toolkit.HoldsFamilies repr Effect fs
    => PossiblyToFn tk (Maybe repr) (Maybe repr) Id.FamilyR
    => CliFriendly tk fs repr Effect
    => HasFallback repr
    => Toolkit tk fs repr Effect
    -> Core.Blessed (State tk p fs repr Effect) -- TODO: the only thing that makes it require `Effect` is `Core.on List.Select` handler, may be there's a way to overcome it ...
    -- -> BlessedOpM (State tk p fs repr m) m Unit
component toolkit =
    B.listAnd Key.library
        [ Box.top $ Offset.px 0
        , Box.left $ Offset.px 0
        , Box.width $ Dimension.px 20
        , Box.height $ Dimension.percents 65.0
        , Box.draggable true
        , Box.scrollable true
        , List.items $ (T.singleLine <<< T.libraryItem (Proxy :: _ tk)) <$> Toolkit.families toolkit
        , List.mouse true
        , List.keys true
        , Box.tags true
        , Style.library
        , Style.libraryBorder
        {- REM
        , Core.on Element.MouseMove
            \_ evt -> do
                -- TODO show family info in status line & documentation
                selected <- List.selected ~< Key.library
                CC.log $ show selected
        -}
        , Core.on List.Select
            \_ _ -> onFamilySelect
        ]
        []
        \_ ->
            pure unit



onFamilySelect
    :: forall tk pstate fs repr m
     . Wiring m
    => FromToRepr repr repr
    => Toolkit.HoldsFamilies repr m fs
    => HasFallback repr
    => PossiblyToFn tk (Maybe repr) (Maybe repr) Id.FamilyR
    => CliFriendly tk fs repr m
    => BlessedOp (State tk pstate fs repr m) m
onFamilySelect =
    do
        state <- State.get

        let mbCurrentPatchR = _.id <$> state.currentPatch
        selected <- List.selected ~< Key.library
        let toolkit = Network.toolkit state.network
        let families = Toolkit.families toolkit
        let mbSelectedFamily = families !! selected

        _ <- case (/\) <$> mbSelectedFamily <*> mbCurrentPatchR of
            Just (familyR /\ curPatchR) ->
                case Toolkit.withAnyFamily
                        (spawnAndRenderRaw
                            toolkit
                            curPatchR
                            familyR
                            $ NodeBox.nextPos state.lastShift)
                        familyR
                        toolkit
                    of
                    Just op -> op
                    Nothing -> pure unit
            Nothing -> pure unit

        pure unit


spawnAndRenderRaw
    :: forall  tk pstate fs repr m
     . Wiring m
    => FromToRepr repr repr
    => Toolkit.HoldsFamilies repr m fs
    => PossiblyToFn tk (Maybe repr) (Maybe repr) Id.FamilyR
    => CliFriendly tk fs repr m
    => HasFallback repr
    => Toolkit tk fs repr m
    -> Id.PatchR
    -> Id.FamilyR
    -> { left :: Int, top :: Int }
    -> Raw.Family repr repr m
    -> BlessedOp (State tk pstate fs repr m) m
spawnAndRenderRaw toolkit patchR familyR nextPos  _ = do
    (mbRawNode :: Maybe (Raw.Node repr repr m)) <- Blessed.lift' $ Toolkit.spawnAnyRaw familyR toolkit

    case mbRawNode of
        Just rawNode -> do
            State.modify_ $ State.withCurrentPatch $ Patch.registerRawNode rawNode
            NodeBox.componentRaw nextPos patchR familyR rawNode
        Nothing -> pure unit


spawnAndRender
    :: forall tk fs pstate f nstate is os repr m
     . Wiring m
    => IsSymbol f
    => FromToRepr nstate repr
    => RegisteredFamily (F f nstate is os repr m) fs
    => PossiblyToFn tk (Maybe repr) (Maybe repr) Id.FamilyR
    => CliFriendly tk fs repr m
    => Toolkit tk fs repr m
    -> Id.PatchR
    -> Id.Family f
    -> { left :: Int, top :: Int }
    -> Toolkit.Family f nstate is os repr m
    -> BlessedOp (State tk pstate fs repr m) m
spawnAndRender toolkit patchR family nextPos  _ = do
    (node :: Noodle.Node f nstate is os repr m) <- Blessed.lift' $ Toolkit.spawn family toolkit
    -- TODO: put node instance in the patch
    NodeBox.component nextPos patchR family node