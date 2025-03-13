module Front.Cli.ApplyNdf where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe(..))

import Control.Monad.State (get, modify) as State

import Data.Traversable (traverse_)

import Noodle.Id (FamilyR, PatchR) as Id
import Noodle.Toolkit (Toolkit)
import Noodle.Toolkit (spawnAnyRaw, class FromPatchState) as Toolkit
import Noodle.Patch (id) as Patch
import Noodle.Raw.Node (id) as RawNode
import Noodle.Fn.Signature (class PossiblyToSignature)
import Noodle.Text.NdfFile (NdfFile)
import Noodle.Text.NdfFile (_normalize, extractCommands) as Ndf
import Noodle.Text.NdfFile.Command (op) as NdfCommand
import Noodle.Text.NdfFile.Command.Op (CommandOp(..)) as Ndf
import Noodle.Text.NdfFile.Types (Coord(..)) as Ndf
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
                        nodeBoxKey >~ Element.setTop  $ Offset.px $ coordY -- inodeBounds.top - 1
                        nodeBoxKey >~ Element.setLeft $ Offset.px $ coordX -- inodeBounds.left
                    Nothing -> pure unit
                pure unit
            Nothing -> pure unit
    _ -> pure unit