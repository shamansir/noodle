module Cli.Components.CommandInput where

import Prelude hiding (show)

import Effect (Effect)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.String (trim) as String
import Data.Bifunctor (bimap)

import Control.Monad.State (modify_, get) as State

import Data.Tuple.Nested ((/\), type (/\))

import Parsing (runParser) as P

import Blessed as B
import Blessed ((>~), (~<))

import Blessed.Core.Dimension as Dimension
import Blessed.Core.Offset as Offset

import Blessed.Internal.Core as Core

import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.UI.Boxes.Box.Option (content, height, left, top, width) as Box
-- import Blessed.UI.Base.Element.Option (index) as Element
import Blessed.UI.Forms.TextArea.Method (setValue)  as TextArea
import Blessed.UI.Base.Element.Method (focus, setFront, hide, show) as Element
import Blessed.UI.Base.Screen.Method (render) as Screen
-- import Blessed.UI.Boxes.Box.Method (focus) as Box
import Blessed.UI.Forms.TextArea.Option (inputOnFocus, mouse) as TextArea
import Blessed.UI.Forms.TextArea.Event (TextAreaEvent(..)) as TextArea
import Blessed.UI.Forms.TextArea.Property (value) as TextArea


import Noodle.Id (FamilyR, unsafeFamilyR, family) as Id
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Fn.Signature (Signature, class PossiblyToSignature)
import Noodle.Toolkit (class FromToPatchState, class HoldsFamilies, isKnownFamily, withAnyFamily) as Toolkit
import Noodle.Repr.HasFallback (class HasFallback, fallback)
import Noodle.Repr.Tagged (class ValueTagged) as VT
import Noodle.Raw.Node (_fromSignature) as RawNode
import Noodle.Patch (id) as Patch
import Noodle.Network (toolkit) as Network
import Noodle.Text.NdfFile.FamilyDef.Parser as NdfFamilyParser
import Noodle.Text.NdfFile.FamilyDef.Codegen (class ParseableRepr)
import Noodle.Text.NdfFile.FamilyDef.Codegen (toRepr, toDefault) as CG
import Noodle.Text.NdfFile.Types (ChannelDef)
import Noodle.Text.NdfFile.Types (encodedTypeOf, encodedValueOf) as CD
import Noodle.Text.NdfFile.Command.FromInput as FI

import Cli.Keys (mainScreen, commandInput, CommandInputKey) as Key
import Cli.State (State)
import Cli.State (currentPatch) as CState
import Cli.Class.CliFriendly (class CliFriendly)
import Cli.Class.CliRenderer (class CliLocator)
import Cli.Style as Style
import Cli.Components.Library as Library
import Cli.Components.NodeBox as NodeBox
import Cli.Components.SidePanel.Console as CC


commandInputKey :: Key.CommandInputKey
commandInputKey = Key.commandInput


component :: forall loc tk pstate fs strepr chrepr
    .  CliLocator loc
    => HasFallback chrepr
    => HasFallback strepr
    => VT.ValueTagged chrepr
    => ParseableRepr chrepr
    => Toolkit.HoldsFamilies strepr chrepr Effect fs
    => Toolkit.FromToPatchState tk pstate strepr
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => CliFriendly tk fs chrepr Effect
    => Core.Blessed (State loc tk pstate fs strepr chrepr Effect)
component =
    B.textBoxAnd commandInputKey
        [ Box.top $ Offset.center
        , Box.left $ Offset.center
        , Box.width $ Dimension.px 60
        , Box.height $ Dimension.px 1
        -- , Box.index 1
        , Style.chInputBox
        , TextArea.mouse true
        , Box.content ""
        , TextArea.inputOnFocus true
        , Core.on TextArea.Submit
            \_ _ -> do
                content <- TextArea.value ~< commandInputKey
                tryExecute $ String.trim content
                hide
        ]
        [  ]
    $ const hide


hide :: forall tk ps fs sr cr mi mo. BlessedOp (State _ tk ps fs sr cr mi) mo
hide = do
    State.modify_ $ _ { commandBoxActive = false }
    commandInputKey >~ Element.hide
    Key.mainScreen >~ Screen.render


show :: forall tk ps fs sr cr mi mo. BlessedOp (State _ tk ps fs sr cr mi) mo
show = do
    commandInputKey >~ Element.setFront
    commandInputKey >~ Element.show
    commandInputKey >~ Element.focus
    -- commandInputKey >~ Box.setContent ""
    commandInputKey >~ TextArea.setValue ""
    State.modify_ $ _ { commandBoxActive = true }
    Key.mainScreen >~ Screen.render


toggle :: forall tk ps fs sr cr mi mo. BlessedOp (State _ tk ps fs sr cr mi) mo
toggle =
    State.get >>= \s ->
        if s.commandBoxActive then
            hide
        else
            show


tryExecute
    :: forall loc tk fs pstate strepr chrepr
     . CliLocator loc
    => HasFallback chrepr
    => HasFallback strepr
    => VT.ValueTagged chrepr
    => ParseableRepr chrepr
    => Toolkit.HoldsFamilies strepr chrepr Effect fs
    => Toolkit.FromToPatchState tk pstate strepr
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => CliFriendly tk fs chrepr Effect
    => String -> BlessedOp (State loc tk pstate fs strepr chrepr Effect) Effect
tryExecute command = do
    state <- State.get
    let
        toolkit = Network.toolkit state.network
        mbCurrentPatch = CState.currentPatch state
        mbCurrentPatchId = Patch.id <$> mbCurrentPatch
    fromInput <- FI.tryExecute toolkit command
    case mbCurrentPatchId of
        Just curPatchR ->
            case fromInput of
                FI.FromFamily familyR rawNode ->
                    Library.registerAndRenderGivenRawNode curPatchR rawNode
                FI.CustomNode signature rawNode ->
                    Library.registerAndRenderGivenRawNode curPatchR rawNode
                FI.CannotSpawn familyR ->
                    CC.log $ "family not found: " <> Id.family familyR
                FI.UnknownCommand command ->
                    CC.log $ "parse error " <> command
        Nothing ->
            CC.log "no current patch"