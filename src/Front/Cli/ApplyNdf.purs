module Front.Cli.ApplyNdf where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log) as Console
import Effect.Aff (runAff_)

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Traversable (traverse_)
import Data.Array (head) as Array
import Data.Newtype (unwrap) as NT

import Control.Monad.State (get, modify, modify_) as State
import Control.Monad.Extra (whenJust)

import Parsing (runParser) as P

import Node.Path (FilePath)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, stat) as Async

import Noodle.Id (FamilyR, PatchR) as Id
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit (spawnAnyRaw, class FromToPatchState) as Toolkit
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
import Noodle.Text.NdfFile.Parser (parser)  as NdfFile
import Noodle.Text.NdfFile.FamilyDef.Codegen (class ParseableRepr, toRepr)
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Repr.ValueInChannel (accept) as ViC
import Noodle.Repr.Tagged (class ValueTagged) as VT
import Noodle.Repr.HasFallback (class HasFallback)

import Blessed ((>~), (~<))
import Blessed.Core.Offset as Offset
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedOp (lift) as Blessed
import Blessed.Internal.BlessedOp (impair1, impair2, configureJs') as Blessed
import Blessed.UI.Base.Element.PropertySet (setTop, setLeft) as Element

import Cli.State (State)
import Cli.State (patch, currentPatchId, findNodeKey, findNodeIdByNdfInstance, registerNdfInstance, findLinkState, appendHistory) as CState
import Cli.Components.Library as Library
import Cli.Components.SidePanel.Console as CC
import Cli.Class.CliFriendly (class CliFriendly)
import Cli.Class.CliRenderer (class CliLocator)

import Front.Cli.Actions as Actions


newtype NdfFilePath = NdfFilePath FilePath
newtype NdfFileContent = NdfFileContent String


applyNdfFileFrom
    :: forall loc tk s fs sr cr
    .  CliLocator loc
    => HasFallback cr
    => VT.ValueTagged cr
    => ParseableRepr cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => Toolkit.FromToPatchState tk s sr
    => CliFriendly tk fs cr Effect
    => Toolkit tk fs sr cr Effect
    -> NdfFilePath
    -> BlessedOp (State loc tk s fs sr cr Effect) Effect
applyNdfFileFrom toolkit (NdfFilePath filePath) = do
    fileCallback <- Blessed.impair1 $ _applyNdfFileContent toolkit <<< map NdfFileContent
    liftEffect $ runAff_ fileCallback $ Async.readTextFile UTF8 filePath


applyNdfFile
    :: forall tk loc s fs sr cr
    .  CliLocator loc
    => HasFallback cr
    => VT.ValueTagged cr
    => ParseableRepr cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => Toolkit.FromToPatchState tk s sr
    => CliFriendly tk fs cr Effect
    => Toolkit tk fs sr cr Effect
    -> Id.PatchR
    -> NdfFile
    -> BlessedOp (State loc tk s fs sr cr Effect) Effect
applyNdfFile toolkit patchR =
    Ndf._normalize
        >>> Ndf.extractCommands
        >>> map NdfCommand.op
        >>> traverse_ (applyNdfCommandOp toolkit patchR)


applyNdfCommandOp
    :: forall tk loc s fs sr cr
    .  CliLocator loc
    => HasFallback cr
    => VT.ValueTagged cr
    => ParseableRepr cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => Toolkit.FromToPatchState tk s sr
    => CliFriendly tk fs cr Effect
    => Toolkit tk fs sr cr Effect
    -> Id.PatchR
    -> Ndf.CommandOp
    -> BlessedOp (State loc tk s fs sr cr Effect) Effect
applyNdfCommandOp toolkit curPatchR = case _ of
    Ndf.MakeNode familyR (Ndf.Coord coordX) (Ndf.Coord coordY) instanceId -> do
        mbRawNode <- Toolkit.spawnAnyRaw familyR toolkit
        whenJust mbRawNode \rawNode -> do
            Library.registerAndRenderGivenRawNode curPatchR rawNode
            state <- State.modify $ CState.registerNdfInstance instanceId $ RawNode.id rawNode
            whenJust (CState.findNodeKey (RawNode.id rawNode) state)
                \nodeBoxKey -> do
                    nodeBoxKey >~ Element.setTop  $ Offset.px $ coordY
                    nodeBoxKey >~ Element.setLeft $ Offset.px $ coordX
                    -- FIXME: record new position in the state
    Ndf.Move instanceId (Ndf.Coord coordX) (Ndf.Coord coordY) -> do
        state <- State.get
        whenJust (CState.findNodeIdByNdfInstance instanceId state >>= flip CState.findNodeKey state)
            \nodeBoxKey -> do
                nodeBoxKey >~ Element.setTop  $ Offset.px $ coordY
                nodeBoxKey >~ Element.setLeft $ Offset.px $ coordX
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
        whenJust mbConnection $
            \{ currentPatch, linkStart, linkEnd } -> do
                Actions.connect Actions.DontTrack linkStart linkEnd currentPatch
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
        whenJust mbConnection $
            \(link /\ linkState) ->
                Actions.disconnect Actions.DontTrack curPatchR link linkState unit
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
        whenJust mbTarget
            \{ inletR, targetNode, valueInChannel } ->
                RawNode.sendIn_ inletR valueInChannel targetNode
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
        whenJust mbTarget
            \{ outletR, targetNode, valueInChannel } ->
                RawNode.sendOut_ outletR valueInChannel targetNode
    Ndf.RemoveNode instanceId -> do
        state <- State.get
        let
            mbNodeR = CState.findNodeIdByNdfInstance instanceId state
            mbNodeBoxKey = mbNodeR >>= \nodeR -> CState.findNodeKey nodeR state
        whenJust ((/\) <$> mbNodeR <*> mbNodeBoxKey)
            \(nodeR /\ nodeBoxKey) ->
                Actions.removeNode Actions.DontTrack curPatchR nodeR nodeBoxKey
    Ndf.Import filePath ->
        applyNdfFileFrom toolkit $ NdfFilePath filePath
    _ -> pure unit


_applyNdfFileContent
    :: forall err loc tk s fs sr cr
    .  CliLocator loc
    => Show err
    => HasFallback cr
    => VT.ValueTagged cr
    => ParseableRepr cr
    => PossiblyToSignature tk (ValueInChannel cr) (ValueInChannel cr) Id.FamilyR
    => Toolkit.FromToPatchState tk s sr
    => CliFriendly tk fs cr Effect
    => Toolkit tk fs sr cr Effect
    -> Either err NdfFileContent
    -> BlessedOp (State loc tk s fs sr cr Effect) Effect
_applyNdfFileContent toolkit (Right (NdfFileContent fileContent)) = do
    case P.runParser fileContent NdfFile.parser of
        Right ndfFile -> do
            state <- State.get
            let mbCurrentPatchId = CState.currentPatchId state
            case mbCurrentPatchId of
                Just curPatchR -> do
                    applyNdfFile toolkit curPatchR ndfFile
                Nothing -> CC.logError "no current patch to apply NDF commands"
            State.modify_ $ CState.appendHistory ndfFile -- we don't normalize it here to keep the history consistent
        Left parsingError -> do
            liftEffect $ Console.log $ "Error : " <> show parsingError
            CC.logError $ show parsingError
_applyNdfFileContent _ (Left error) = do
    liftEffect $ Console.log $ "Error : " <> show error
    CC.logError $ show error