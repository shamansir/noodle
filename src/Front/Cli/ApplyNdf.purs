module Front.Cli.ApplyNdf where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Traversable (traverse_)
import Data.Array (head) as Array
import Data.Newtype (unwrap) as NT

import Control.Monad.State (get, modify) as State

import Noodle.Id (FamilyR, PatchR) as Id
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit (spawnAnyRaw, class FromPatchState) as Toolkit
import Noodle.Patch (id, connectRaw, disconnectRaw, findRawNode, linksMap) as Patch
import Noodle.Patch.Links (findBetween) as Links
import Noodle.Raw.Node (id, shape, sendIn_, sendOut_) as RawNode
import Noodle.Raw.Link (id) as RawLink
import Noodle.Fn.Signature (class PossiblyToSignature)
import Noodle.Fn.Shape as Shape
import Noodle.Raw.Fn.Shape as RawShape
import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile (_normalize, extractCommands) as Ndf
import Noodle.Text.NdfFile.Command (op) as NdfCommand
import Noodle.Text.NdfFile.Command.Op (CommandOp(..)) as Ndf
import Noodle.Text.NdfFile.Types (Coord(..), InletId(..), OutletId(..), EncodedType(..), EncodedValue(..), findInlet, findOutlet) as Ndf
import Noodle.Text.NdfFile.FamilyDef.Codegen (class ParseableRepr, toRepr)
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.ValueInChannel (accept) as ViC
import Noodle.Repr.Tagged (class ValueTagged) as VT
import Noodle.Repr.HasFallback (class HasFallback)

import Blessed ((>~), (~<))
import Blessed.Core.Offset as Offset
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedOp (lift) as Blessed
import Blessed.UI.Base.Element.PropertySet (setTop, setLeft) as Element

import Cli.State (State)
import Cli.State (patch, findNodeKey, findNodeIdByNdfInstance, registerNdfInstance, findLinkState) as CState
import Cli.Components.Library as Library
import Cli.Components.SidePanel.Console as CC
import Cli.Class.CliFriendly (class CliFriendly)
import Cli.Class.CliRenderer (class CliLocator)


import Front.Cli.Actions as Actions


applyNdf
    :: forall tk loc s fs sr cr
    .  CliLocator loc
    => HasFallback cr
    => VT.ValueTagged cr
    => ParseableRepr cr
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
    => ParseableRepr cr
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
                        -- FIXME: record new position in the state
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
                currentPatch <- CState.patch curPatchR state
                fromNodeR    <- CState.findNodeIdByNdfInstance fromInstanceId state
                toNodeR      <- CState.findNodeIdByNdfInstance toInstanceId state
                sourceNode   <- Patch.findRawNode fromNodeR currentPatch
                targetNode   <- Patch.findRawNode toNodeR currentPatch
                srcNodeKey   <- CState.findNodeKey fromNodeR state
                trgNodeKey   <- CState.findNodeKey toNodeR state
                outletR      <- Ndf.findOutlet ndfOutletId $ RawNode.shape sourceNode
                inletR       <- Ndf.findInlet ndfInletId $ RawNode.shape targetNode
                outletIndex  <- RawShape.indexOfOutlet outletR $ RawNode.shape sourceNode
                inletIndex   <- RawShape.indexOfInlet  inletR  $ RawNode.shape targetNode
                pure
                    { currentPatch
                    , linkStart : { key : srcNodeKey, node : sourceNode, outlet : outletR, outletIndex : outletIndex }
                    , linkEnd :   { key : trgNodeKey, node : targetNode, inlet  : inletR,  inletIndex :  inletIndex  }
                    }
        case mbConnection of
            Just { currentPatch, linkStart, linkEnd } -> do
                Actions.connect Actions.DontTrack linkStart linkEnd currentPatch
            Nothing -> pure unit
        pure unit
    Ndf.Disconnect fromInstanceId ndfOutletId toInstanceId ndfInletId -> do
        state <- State.get
        let
            mbConnection = do
                currentPatch <- CState.patch curPatchR state
                fromNodeR    <- CState.findNodeIdByNdfInstance fromInstanceId state
                toNodeR      <- CState.findNodeIdByNdfInstance toInstanceId state
                sourceNode   <- Patch.findRawNode fromNodeR currentPatch
                targetNode   <- Patch.findRawNode toNodeR currentPatch
                outletR      <- Ndf.findOutlet ndfOutletId $ RawNode.shape sourceNode
                inletR       <- Ndf.findInlet ndfInletId $ RawNode.shape targetNode
                topLink      <- Patch.linksMap currentPatch # Links.findBetween (fromNodeR /\ outletR) (toNodeR /\ inletR) # Array.head
                linkState    <- CState.findLinkState (RawLink.id topLink) state
                pure $ topLink /\ linkState
        case mbConnection of
            Just (link /\ linkState) ->
                Actions.disconnect Actions.DontTrack curPatchR link linkState unit
            Nothing -> pure unit
    Ndf.Send instanceId ndfInletId encodedValue -> do
        state <- State.get
        let
            mbTarget = do
                currentPatch <- CState.patch curPatchR state
                nodeR <- CState.findNodeIdByNdfInstance instanceId state
                targetNode <- Patch.findRawNode nodeR currentPatch
                inletR <- Ndf.findInlet ndfInletId $ RawNode.shape targetNode
                valueTag <- RawShape.tagOfInlet inletR $ RawNode.shape targetNode
                reprValue <- toRepr (Ndf.EncodedType $ NT.unwrap valueTag) encodedValue
                pure { inletR, targetNode, valueInChannel : ViC.accept reprValue }
        case mbTarget of
            Just { inletR, targetNode, valueInChannel } ->
                RawNode.sendIn_ inletR valueInChannel targetNode
            Nothing -> pure unit
    Ndf.SendO instanceId ndfOutletId encodedValue -> do
        state <- State.get
        let
            mbTarget = do
                currentPatch <- CState.patch curPatchR state
                nodeR <- CState.findNodeIdByNdfInstance instanceId state
                targetNode <- Patch.findRawNode nodeR currentPatch
                outletR <- Ndf.findOutlet ndfOutletId $ RawNode.shape targetNode
                valueTag <- RawShape.tagOfOutlet outletR $ RawNode.shape targetNode
                reprValue <- toRepr (Ndf.EncodedType $ NT.unwrap valueTag) encodedValue
                pure { outletR, targetNode, valueInChannel : ViC.accept reprValue }
        case mbTarget of
            Just { outletR, targetNode, valueInChannel } ->
                RawNode.sendOut_ outletR valueInChannel targetNode
            Nothing -> pure unit
    Ndf.RemoveNode instanceId -> do
        state <- State.get
        let
            mbNodeR = CState.findNodeIdByNdfInstance instanceId state
            mbNodeBoxKey = mbNodeR >>= \nodeR -> CState.findNodeKey nodeR state
        case (/\) <$> mbNodeR <*> mbNodeBoxKey of
            Just (nodeR /\ nodeBoxKey) ->
                Actions.removeNode Actions.DontTrack curPatchR nodeR nodeBoxKey
            Nothing -> pure unit
    Ndf.Import filePath ->
        pure unit
    _ -> pure unit