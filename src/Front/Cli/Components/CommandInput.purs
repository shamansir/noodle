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


import Noodle.Id (FamilyR, unsafeFamilyR) as Id
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Fn.Signature (Signature, class PossiblyToSignature)
import Noodle.Toolkit (class FromPatchState, class HoldsFamilies, isKnownFamily, withAnyFamily) as Toolkit
import Noodle.Repr.HasFallback (class HasFallback, fallback)
import Noodle.Repr.Tagged (class Tagged) as CT
import Noodle.Raw.Node (_fromSignature) as RawNode
import Noodle.Patch (id) as Patch
import Noodle.Network (toolkit) as Network
import Noodle.Text.NdfFile.FamilyDef.Parser as NdfFamilyParser
import Noodle.Text.NdfFile.FamilyDef.Codegen (class ParseableRepr)
import Noodle.Text.NdfFile.FamilyDef.Codegen (toRepr, toDefault) as CG
import Noodle.Text.NdfFile.Types (ChannelDef)
import Noodle.Text.NdfFile.Types (encodedTypeOf, encodedValueOf) as CD

import Cli.Keys (mainScreen, commandInput, CommandInputKey) as Key
import Cli.State (State)
import Cli.State (currentPatch) as CState
import Cli.Class.CliFriendly (class CliFriendly)
import Cli.Style as Style
import Cli.Components.Library as Library
import Cli.Components.NodeBox as NodeBox
import Cli.Components.SidePanel.Console as CC


commandInputKey :: Key.CommandInputKey
commandInputKey = Key.commandInput


component :: forall tk pstate fs strepr chrepr
    .  HasFallback chrepr
    => HasFallback strepr
    => CT.Tagged chrepr
    => ParseableRepr chrepr
    => Toolkit.HoldsFamilies strepr chrepr Effect fs
    => Toolkit.FromPatchState tk pstate strepr
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => CliFriendly tk fs chrepr Effect
    => Core.Blessed (State tk pstate fs strepr chrepr Effect)
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


hide :: forall tk ps fs sr cr mi mo. BlessedOp (State tk ps fs sr cr mi) mo
hide = do
    State.modify_ $ _ { commandBoxActive = false }
    commandInputKey >~ Element.hide
    Key.mainScreen >~ Screen.render


show :: forall tk ps fs sr cr mi mo. BlessedOp (State tk ps fs sr cr mi) mo
show = do
    commandInputKey >~ Element.setFront
    commandInputKey >~ Element.show
    commandInputKey >~ Element.focus
    -- commandInputKey >~ Box.setContent ""
    commandInputKey >~ TextArea.setValue ""
    State.modify_ $ _ { commandBoxActive = true }
    Key.mainScreen >~ Screen.render


toggle :: forall tk ps fs sr cr mi mo. BlessedOp (State tk ps fs sr cr mi) mo
toggle =
    State.get >>= \s ->
        if s.commandBoxActive then
            hide
        else
            show


tryExecute
    :: forall tk fs pstate strepr chrepr
     . HasFallback chrepr
    => HasFallback strepr
    => CT.Tagged chrepr
    => ParseableRepr chrepr
    => Toolkit.HoldsFamilies strepr chrepr Effect fs
    => Toolkit.FromPatchState tk pstate strepr
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => CliFriendly tk fs chrepr Effect
    => String -> BlessedOp (State tk pstate fs strepr chrepr Effect) Effect
tryExecute command = do
    state <- State.get
    let
        toolkit = Network.toolkit state.network
        mbCurrentPatch = CState.currentPatch state
        mbCurrentPatchId = Patch.id <$> mbCurrentPatch
    case (/\) <$> Toolkit.isKnownFamily command toolkit <*> mbCurrentPatchId of
        Just (familyR /\ curPatchR) ->
            case Toolkit.withAnyFamily
                    (Library.spawnAndRenderRaw
                        toolkit
                        curPatchR
                        familyR
                        $ NodeBox.nextPos state.lastShift)
                    familyR
                    toolkit
                of
                Just op -> op
                Nothing -> pure unit
        Nothing ->
            case P.runParser command $ NdfFamilyParser.fnSignature "custom" of
                -- (StateDef /\ Fn ChannelDef ChannelDef)
                Right (stateDef /\ fn) -> do
                    case mbCurrentPatchId of
                        Just curPatchR -> do
                            CC.log $ "got fn: " <> command
                            rawNode <- RawNode._fromSignature (Id.unsafeFamilyR "custom") (fallback :: strepr) (convertFn fn) $ pure unit
                            Library.spawnAndRenderGivenRawNode curPatchR (NodeBox.nextPos state.lastShift) rawNode
                        Nothing -> do
                            CC.log "no patch ID"
                            pure unit
                Left err -> CC.log $ "parse error " <> command
    pure unit
    where
        defToRepr :: ChannelDef -> chrepr
        defToRepr chanDef =
            case CD.encodedTypeOf chanDef of
                Just encodedType ->
                    case CD.encodedValueOf chanDef of
                        Just encodedValue ->
                            CG.toRepr encodedType encodedValue # fromMaybe fallback
                        Nothing -> CG.toDefault encodedType
                Nothing -> fallback

        convertFn :: Signature ChannelDef ChannelDef -> Signature chrepr chrepr
        convertFn = bimap defToRepr defToRepr

    {-
    state <- State.get
    let mbCurrentPatch = CState.currentPatch state
    (mbPatchState :: Maybe pstate) <- CState.currentPatchState =<< State.get
    case Patch.id <$> mbCurrentPatch of
        Just patchR -> do
            let familyR = Id.unsafeFamilyR "custom"
            let (mbNodeState :: Maybe strepr) = mbPatchState >>= Toolkit.loadFromPatch (Proxy :: _ tk) familyR
            case mbNodeState of
                Just nodeState -> do
                    let
                        shape =
                            RawShape.make
                                { inlets :
                                    [ { name : Id.unsafeInletR "foo", order : 0, temp : Hot, tag : RawShape.tagAs "Number" }
                                    , { name : Id.unsafeInletR "bar", order : 1, temp : Hot, tag : RawShape.tagAs "Number" }
                                    ]
                                , outlets : []
                                }
                    let inletsMap = Map.empty
                                        # Map.insert (Id.unsafeInletR "foo") fallback
                                        # Map.insert (Id.unsafeInletR "bar") fallback
                    (rawNode :: Raw.Node strepr chrepr Effect) <- RawNode.make (Id.unsafeFamilyR "custom") nodeState shape inletsMap Map.empty $ pure unit
                    Library.spawnAndRenderGivenRawNode patchR { top : 20, left : 20 } rawNode
                    pure unit
                Nothing -> pure unit
        Nothing ->
            pure unit
    -}