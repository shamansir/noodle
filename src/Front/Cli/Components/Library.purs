module Cli.Components.Library where


import Control.Monad.State (get, modify_) as State
import Control.Monad.Extra (whenJust)

import Type.Proxy (Proxy(..))
import Type.Data.Symbol (class IsSymbol)

import Effect (Effect)

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Array ((!!))
import Data.Traversable (traverse_)
import Data.Map (empty, insert) as Map
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
import Cli.State (withCurrentPatch, currentPatchState, currentPatch) as CState
import Cli.Style (library, libraryBorder) as Style
import Cli.Class.CliFriendly (class CliFriendly)
import Cli.Class.CliRenderer (class CliLocator)

import Noodle.Fn.Shape.Temperament (Temperament(..))
import Noodle.Id (PatchR, FamilyR, Family, familyR, familyOf, unsafeFamilyR, unsafeInletR, unsafeOutletR) as Id
import Noodle.Repr.HasFallback (class HasFallback, fallback)
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.StRepr (from) as StRepr
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Network (toolkit) as Network
import Noodle.Toolkit (class HoldsFamilies, families, spawn, spawnAnyRaw, withAnyFamily, class FromPatchState, loadFromPatch, withFamilyUnsafe, withRawFamily) as Toolkit
import Noodle.Toolkit.Family (Family, familyIdOf) as Toolkit
import Noodle.Toolkit.Families (F, class RegisteredFamily)
import Noodle.Toolkit (Toolkit)
import Noodle.Ui.Tagging (libraryItem) as T
import Noodle.Wiring (class Wiring)
import Noodle.Fn.Signature (class PossiblyToSignature)
import Noodle.Node (Node) as Noodle
import Noodle.Node (id, setState) as Node
import Noodle.Patch (id, registerNode, registerRawNode) as Patch
import Noodle.Raw.Node (Node) as Raw
import Noodle.Raw.Node (id, make, setState, sendIn) as RawNode
import Noodle.Raw.Fn.Shape (make, empty, tagAs) as RawShape
import Noodle.Raw.Toolkit.Family (Family) as Raw
import Noodle.Text.NdfFile.Command.Quick as QOp
import Noodle.Repr.Tagged (class ValueTagged) as VT

import Cli.Components.NodeBox as NodeBox
import Cli.Components.SidePanel as SidePanel
import Cli.Components.SidePanel.CommandLog as CL
import Cli.Components.SidePanel.Tree as TP



import Prelude


component
    :: forall loc tk ps fs strepr chrepr
     . CliLocator loc
    => HasFallback chrepr
    => VT.ValueTagged chrepr
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => Toolkit.HoldsFamilies strepr chrepr Effect fs
    => Toolkit.FromPatchState tk ps strepr
    => CliFriendly tk fs chrepr Effect
    => Toolkit tk fs strepr chrepr Effect
    -> Core.Blessed (State loc tk ps fs strepr chrepr Effect) -- TODO: the only thing that makes it require `Effect` is `Core.on List.Select` handler, may be there's a way to overcome it ...
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
        \_ -> pure unit



onFamilySelect
    :: forall loc tk pstate fs strepr chrepr m
     . Wiring m
    => CliLocator loc
    => HasFallback chrepr
    => VT.ValueTagged chrepr
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => Toolkit.HoldsFamilies strepr chrepr m fs
    => Toolkit.FromPatchState tk pstate strepr
    => CliFriendly tk fs chrepr m
    => BlessedOp (State loc tk pstate fs strepr chrepr m) m
onFamilySelect =
    do
        state <- State.get

        let mbCurrentPatchR = _.id <$> state.currentPatch
        selected <- List.selected ~< Key.library
        let toolkit = Network.toolkit state.network
        let families = Toolkit.families toolkit
        let mbSelectedFamily = families !! selected
        _ <- whenJust ((/\) <$> mbSelectedFamily <*> mbCurrentPatchR)
            \(familyR /\ curPatchR) ->
                case Toolkit.withFamilyUnsafe
                        (spawnAndRender
                            toolkit
                            curPatchR
                        ) familyR toolkit of
                    Just op -> op
                    Nothing ->
                        whenJust
                            (Toolkit.withRawFamily -- Toolkit.withAnyFamily
                                (spawnAndRenderRaw
                                    toolkit
                                    curPatchR
                                    familyR
                                )
                                familyR
                                toolkit
                            )
                            identity
        pure unit


spawnAndRenderRaw
    :: forall loc tk pstate fs strepr chrepr m
     . Wiring m
    => CliLocator loc
    => HasFallback chrepr
    => VT.ValueTagged chrepr
    => Toolkit.HoldsFamilies strepr chrepr m fs
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => Toolkit.FromPatchState tk pstate strepr
    => CliFriendly tk fs chrepr m
    => Toolkit tk fs strepr chrepr m
    -> Id.PatchR
    -> Id.FamilyR
    -> Raw.Family strepr chrepr m
    -> BlessedOp (State loc tk pstate fs strepr chrepr m) m
spawnAndRenderRaw toolkit patchR familyR _ = do
    (mbRawNode :: Maybe (Raw.Node strepr chrepr m)) <- Blessed.lift' $ Toolkit.spawnAnyRaw familyR toolkit

    whenJust mbRawNode \rawNode -> do
        registerAndRenderGivenRawNode patchR rawNode


registerAndRenderGivenRawNode
    :: forall loc tk pstate fs strepr chrepr m
     . Wiring m
    => CliLocator loc
    => HasFallback chrepr
    => VT.ValueTagged chrepr
    -- => Toolkit.HoldsFamilies strepr chrepr m fs
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => Toolkit.FromPatchState tk pstate strepr
    => CliFriendly tk fs chrepr m
    -- => Toolkit tk fs strepr chrepr m
    => Id.PatchR
    -> Raw.Node strepr chrepr m
    -> BlessedOp (State loc tk pstate fs strepr chrepr m) m
registerAndRenderGivenRawNode patchR rawNode = do
    let familyR = Id.familyOf $ RawNode.id rawNode

    -- we load the default initial node state for the family
    (mbPatchState :: Maybe pstate) <- CState.currentPatchState =<< State.get
    let (mbNodeState :: Maybe strepr) = mbPatchState >>= Toolkit.loadFromPatch (Proxy :: _ tk) familyR

    whenJust mbNodeState
        \nextState -> rawNode # RawNode.setState nextState

    State.modify_ $ CState.withCurrentPatch $ Patch.registerRawNode rawNode

    nextPos <- NodeBox.componentRaw patchR familyR rawNode

    CL.trackCommand $ QOp.makeNode (RawNode.id rawNode) nextPos
    SidePanel.refresh TP.sidePanel

    Key.mainScreen >~ Screen.render


spawnAndRender
    :: forall loc tk fs pstate f fstate is os strepr chrepr m
     . Wiring m
    => CliLocator loc
    => IsSymbol f
    => HasFallback chrepr
    => HasFallback fstate
    => VT.ValueTagged chrepr
    => StRepr fstate strepr
    => RegisteredFamily (F f fstate is os chrepr m) fs
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => Toolkit.FromPatchState tk pstate strepr
    => CliFriendly tk fs chrepr m
    => Toolkit tk fs strepr chrepr m
    -> Id.PatchR
    -> Toolkit.Family f fstate is os chrepr m
    -> BlessedOp (State loc tk pstate fs strepr chrepr m) m
spawnAndRender toolkit patchR family = do
    let (familyId :: Id.Family f) = (Toolkit.familyIdOf family)

    (node :: Noodle.Node f fstate is os chrepr m) <- Blessed.lift' $ Toolkit.spawn familyId toolkit
    (mbPatchState :: Maybe pstate) <- CState.currentPatchState =<< State.get
    let (mbNodeState :: Maybe strepr) = mbPatchState >>= Toolkit.loadFromPatch (Proxy :: _ tk) (Id.familyR familyId)

    whenJust (mbNodeState >>= StRepr.from) $ flip Node.setState node

    State.modify_ $ CState.withCurrentPatch $ Patch.registerNode node

    nextPos <- NodeBox.component patchR familyId node

    CL.trackCommand $ QOp.makeNode (Node.id node) nextPos
    SidePanel.refresh TP.sidePanel

    Key.mainScreen >~ Screen.render