module Cli.Components.Library where


import Control.Monad.State (get, modify_) as State

import Type.Proxy (Proxy(..))
import Type.Data.Symbol (class IsSymbol)

import Effect (Effect)

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Array ((!!))
import Data.Traversable (traverse_)

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
import Blessed.UI.Base.Screen.Method (render) as Screen

import Cli.Keys as Key
import Cli.State (State)
import Cli.State (withCurrentPatch, currentPatchState) as CState
import Cli.Style (library, libraryBorder) as Style
import Cli.Panels as Panels
import Cli.Class.CliFriendly (class CliFriendly)

import Noodle.Id (PatchR, FamilyR, Family, familyR) as Id
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.StRepr (from) as StRepr
import Noodle.Repr.ValueInChannel (ValueInChannel, class ToValueInChannel)
import Noodle.Network (toolkit) as Network
import Noodle.Toolkit (class HoldsFamilies, families, spawn, spawnAnyRaw, withAnyFamily, class FromPatchState, loadFromPatch) as Toolkit
import Noodle.Toolkit.Family (Family) as Toolkit
import Noodle.Toolkit.Families (F, class RegisteredFamily)
import Noodle.Toolkit (Toolkit)
import Noodle.Ui.Cli.Tagging (libraryItem) as T
import Noodle.Wiring (class Wiring)
import Noodle.Fn.ToFn (class PossiblyToFn)
import Noodle.Node (Node) as Noodle
import Noodle.Node (id, setState) as Node
import Noodle.Patch (registerNode, registerRawNode) as Patch
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (id, setState) as RawNode
import Noodle.Raw.Toolkit.Family (Family) as Raw
import Noodle.Text.NdfFile.Command.Quick as QOp

import Cli.Components.NodeBox as NodeBox
import Cli.Components.SidePanel as SP
import Cli.Components.SidePanel.CommandLog as CL



import Prelude


component
    :: forall tk ps fs strepr chrepr
     . HasFallback chrepr
    => PossiblyToFn tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => Toolkit.HoldsFamilies strepr chrepr Effect fs
    => Toolkit.FromPatchState tk ps strepr
    => CliFriendly tk fs chrepr Effect
    => Toolkit tk fs strepr chrepr Effect
    -> Core.Blessed (State tk ps fs strepr chrepr Effect) -- TODO: the only thing that makes it require `Effect` is `Core.on List.Select` handler, may be there's a way to overcome it ...
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
    :: forall tk pstate fs strepr chrepr m
     . Wiring m
    => HasFallback chrepr
    => PossiblyToFn tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => Toolkit.HoldsFamilies strepr chrepr m fs
    => Toolkit.FromPatchState tk pstate strepr
    => CliFriendly tk fs chrepr m
    => BlessedOp (State tk pstate fs strepr chrepr m) m
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
    :: forall  tk pstate fs strepr chrepr m
     . Wiring m
    => Toolkit.HoldsFamilies strepr chrepr m fs
    => PossiblyToFn tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => Toolkit.FromPatchState tk pstate strepr
    => CliFriendly tk fs chrepr m
    => HasFallback chrepr
    => Toolkit tk fs strepr chrepr m
    -> Id.PatchR
    -> Id.FamilyR
    -> { left :: Int, top :: Int }
    -> Raw.Family strepr chrepr m
    -> BlessedOp (State tk pstate fs strepr chrepr m) m
spawnAndRenderRaw toolkit patchR familyR nextPos  _ = do
    (mbRawNode :: Maybe (Raw.Node strepr chrepr m)) <- Blessed.lift' $ Toolkit.spawnAnyRaw familyR toolkit

    case mbRawNode of
        Just rawNode -> do
            (mbPatchState :: Maybe pstate) <- CState.currentPatchState =<< State.get
            let (mbNodeState :: Maybe strepr) = mbPatchState >>= Toolkit.loadFromPatch (Proxy :: _ tk) familyR

            case mbNodeState of
                Just nextState -> rawNode # RawNode.setState nextState
                Nothing -> pure unit

            State.modify_ $ CState.withCurrentPatch $ Patch.registerRawNode rawNode

            NodeBox.componentRaw nextPos patchR familyR rawNode

            CL.trackCommand $ QOp.makeNode (RawNode.id rawNode) nextPos

            Key.mainScreen >~ Screen.render
        Nothing -> pure unit


spawnAndRender
    :: forall tk fs pstate f fstate is os strepr chrepr m
     . Wiring m
    => IsSymbol f
    => HasFallback chrepr
    => HasFallback fstate
    => StRepr fstate strepr
    => RegisteredFamily (F f fstate is os chrepr m) fs
    => PossiblyToFn tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => Toolkit.FromPatchState tk pstate strepr
    => CliFriendly tk fs chrepr m
    => Toolkit tk fs strepr chrepr m
    -> Id.PatchR
    -> Id.Family f
    -> { left :: Int, top :: Int }
    -> Toolkit.Family f fstate is os chrepr m
    -> BlessedOp (State tk pstate fs strepr chrepr m) m
spawnAndRender toolkit patchR family nextPos  _ = do
    (node :: Noodle.Node f fstate is os chrepr m) <- Blessed.lift' $ Toolkit.spawn family toolkit
    (mbPatchState :: Maybe pstate) <- CState.currentPatchState =<< State.get
    let (mbNodeState :: Maybe strepr) = mbPatchState >>= Toolkit.loadFromPatch (Proxy :: _ tk) (Id.familyR family)

    traverse_ (flip Node.setState node) $ StRepr.from =<< mbNodeState

    State.modify_ $ CState.withCurrentPatch $ Patch.registerNode node

    NodeBox.component nextPos patchR family node

    CL.trackCommand $ QOp.makeNode (Node.id node) nextPos

    Key.mainScreen >~ Screen.render