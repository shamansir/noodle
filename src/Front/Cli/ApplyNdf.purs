module Front.Cli.ApplyNdf where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)

import Data.Maybe (Maybe(..))

import Control.Monad.State (get, modify) as State

import Data.Traversable (traverse_)

import Noodle.Id (FamilyR, PatchR) as Id
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit (spawnAnyRaw, class FromPatchState) as Toolkit
import Noodle.Patch (id, connectRaw, disconnectRaw, findRawNode) as Patch
import Noodle.Raw.Node (id, shape) as RawNode
import Noodle.Fn.Signature (class PossiblyToSignature)
import Noodle.Fn.Shape as RawShape
import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile (_normalize, extractCommands) as Ndf
import Noodle.Text.NdfFile.Command (op) as NdfCommand
import Noodle.Text.NdfFile.Command.Op (CommandOp(..)) as Ndf
import Noodle.Text.NdfFile.Types (Coord(..), InletId(..), OutletId(..), EncodedValue(..), findInlet, findOutlet) as Ndf
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.Tagged (class ValueTagged) as VT
import Noodle.Repr.HasFallback (class HasFallback)

import Blessed ((>~), (~<))
import Blessed.Core.Offset as Offset
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.UI.Base.Element.PropertySet (setTop, setLeft) as Element

import Cli.State (State)
import Cli.State (currentPatch, findNodeKey, findNodeIdByNdfInstance, registerNdfInstance) as CState
import Cli.Components.Library as Library
import Cli.Components.SidePanel.Console as CC
import Cli.Class.CliFriendly (class CliFriendly)
import Cli.Class.CliRenderer (class CliLocator)



applyNdf
    :: forall tk loc s fs sr cr
    .  CliLocator loc
    => HasFallback cr
    => VT.ValueTagged cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => Toolkit.FromPatchState tk s sr
    => CliFriendly tk fs cr Effect
    => Toolkit tk fs sr cr Effect
    -> Id.PatchR
    -> NdfFile
    -> BlessedOp (State loc tk s fs sr cr Effect) Effect
applyNdf toolkit patchR  =
    Ndf._normalize
        >>> Ndf.extractCommands
        >>> map NdfCommand.op
        >>> traverse_ (applyCommandOp toolkit patchR)


applyCommandOp
    :: forall tk loc s fs sr cr
    .  CliLocator loc
    => HasFallback cr
    => VT.ValueTagged cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => Toolkit.FromPatchState tk s sr
    => CliFriendly tk fs cr Effect
    => Toolkit tk fs sr cr Effect
    -> Id.PatchR
    -> Ndf.CommandOp
    -> BlessedOp (State loc tk s fs sr cr Effect) Effect
applyCommandOp toolkit curPatchR = case _ of
    Ndf.MakeNode familyR (Ndf.Coord coordX) (Ndf.Coord coordY) instanceId -> do
        mbRawNode <- Toolkit.spawnAnyRaw familyR toolkit
        case mbRawNode of
            Just rawNode -> do
                Library.registerAndRenderGivenRawNode curPatchR rawNode
                state <- State.modify $ CState.registerNdfInstance instanceId $ RawNode.id rawNode
                case CState.findNodeKey (RawNode.id rawNode) state of
                    Just nodeBoxKey -> do
                        nodeBoxKey >~ Element.setTop  $ Offset.px $ coordY
                        nodeBoxKey >~ Element.setLeft $ Offset.px $ coordX
                    Nothing -> pure unit
                pure unit
            Nothing -> pure unit
    Ndf.Move instanceId (Ndf.Coord coordX) (Ndf.Coord coordY) -> do
        state <- State.get
        case CState.findNodeIdByNdfInstance instanceId state >>= flip CState.findNodeKey state of
            Just nodeBoxKey -> do
                nodeBoxKey >~ Element.setTop  $ Offset.px $ coordY
                nodeBoxKey >~ Element.setLeft $ Offset.px $ coordX
            Nothing -> pure unit
    Ndf.Connect fromInstanceId ndfOutletId toInstanceId ndfInletId -> do
        state <- State.get
        let
            mbConnection = do
                currentPatch <- CState.currentPatch state
                fromNodeR    <- CState.findNodeIdByNdfInstance fromInstanceId state
                toNodeR      <- CState.findNodeIdByNdfInstance toInstanceId state
                sourceNode   <- Patch.findRawNode fromNodeR currentPatch
                targetNode   <- Patch.findRawNode toNodeR currentPatch
                outletR      <- Ndf.findOutlet ndfOutletId $ RawNode.shape sourceNode
                inletR       <- Ndf.findInlet ndfInletId $ RawNode.shape targetNode
                pure { currentPatch, sourceNode, targetNode, outletR, inletR }
        case mbConnection of
            Just { currentPatch, sourceNode, targetNode, outletR, inletR } -> do
                _ <- liftEffect $ Patch.connectRaw outletR inletR sourceNode targetNode currentPatch
                pure unit
            Nothing -> pure unit
        pure unit
    Ndf.Disconnect fromInstanceId (Ndf.OutletId ndfOutletId) toInstanceId (Ndf.InletId ndfInletId) ->
        pure unit
    Ndf.Send instanceId (Ndf.InletId inletId) (Ndf.EncodedValue encodedValue) ->
        pure unit
    Ndf.SendO instanceId (Ndf.OutletId outletId) (Ndf.EncodedValue encodedValue) ->
        pure unit
    Ndf.RemoveNode instanceId ->
        pure unit
    Ndf.Import filePath ->
        pure unit
    _ -> pure unit