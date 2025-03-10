module Cli.Components.MainScreen where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console

import Data.List (toUnfoldable) as List
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))

import Blessed ((>~))
import Blessed as B
import Blessed (exit) as Blessed

import Blessed.Internal.Core as Core
import Blessed.Core.Key (alpha, control, escape, tab) as Key

import Blessed.UI.Base.Screen.Event (key) as Screen
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Base.Screen.Option as Screen
import Blessed.UI.Boxes.Box.Method as Box

import Noodle.Id as Id
import Noodle.Network as Network
import Noodle.Toolkit as Toolkit
import Noodle.Repr.HasFallback (class HasFallback)
import Noodle.Repr.StRepr (class StRepr)
import Noodle.Repr.Tagged (class ValueTagged) as VT
import Noodle.Repr.ValueInChannel (ValueInChannel)
import Noodle.Fn.Signature (class PossiblyToSignature)
import Noodle.Text.NdfFile.FamilyDef.Codegen (class ParseableRepr)

import Noodle.Ui.Cli.Tagging.At as T
import Noodle.Ui.Cli.Tagging.At (ChannelLabel) as At

import Cli.Keys (mainScreen, library) as Key
import Cli.State (State)
import Cli.State (formatHistory, isPanelOn) as CState
import Cli.Panels (Which(..)) as P
import Cli.Class.CliFriendly (class CliFriendly)

import Cli.Components.SidePanel as SP

import Cli.Components.PatchBox as PatchBox
import Cli.Components.PatchesListbar as PatchesListbar
import Cli.Components.AddPatchButton as AddPatchButton
-- import Cli.Components.LoadFileButton as LoadFileButton
import Cli.Components.SidePanel.Documentation (sidePanel) as Doc
import Cli.Components.SidePanel.CommandLog (sidePanel) as CL
import Cli.Components.SidePanel.Console (sidePanel) as Console
import Cli.Components.SidePanel.WsServerStatus (sidePanel) as WS
import Cli.Components.SidePanel.Tree (sidePanel) as Tree
-- import Cli.Components.SidePanel.HydraCode (sidePanel) as HC
-- import Cli.Components.PaletteList as PaletteList
import Cli.Components.StatusLine as StatusLine
import Cli.Components.CommandInput as CommandInput


-- import Toolkit.Hydra (toolkit, Toolkit) as Hydra


-- families :: Array Id.FamilyR
-- families = List.toUnfoldable $ Toolkit.nodeFamilies (Hydra.toolkit :: Hydra.Toolkit Effect)


-- TODO: take toolkit here
component
    :: forall tk ps fs strepr chrepr
    .  HasFallback chrepr
    => HasFallback strepr
    => VT.ValueTagged chrepr
    => ParseableRepr chrepr
    => Toolkit.HoldsFamilies strepr chrepr Effect fs
    => Toolkit.FromPatchState tk ps strepr
    => PossiblyToSignature tk (ValueInChannel chrepr) (ValueInChannel chrepr) Id.FamilyR
    => CliFriendly tk fs chrepr Effect
    => State tk ps fs strepr chrepr Effect
    -> Core.Blessed (State tk ps fs strepr chrepr Effect)
component initialState =
    B.screenAnd Key.mainScreen

        [ Screen.title "Noodle"
        , Screen.smartCSR true
        , Screen.fullUnicode true
        , Screen.key
            [ Key.escape, Key.alpha 'q', (Key.control $ Key.alpha 'C') ]
            $ \_ kevt -> do
                Blessed.exit
        , Screen.key
            [ Key.tab ]
            $ \_ kevt -> do
                CommandInput.toggle
        ]

        (
        [ PatchesListbar.component $ Network.patches $ initialState.network
        , AddPatchButton.component
        , PatchBox.component $ Network.toolkit initialState.network
        -- , LoadFileButton.component
        , SP.button 11 (isOnByDefault P.Documentation) Doc.sidePanel
        , SP.button 9 (isOnByDefault P.Commands)      CL.sidePanel
        , SP.button 7 (isOnByDefault P.Tree)          Tree.sidePanel
        , SP.button 5 (isOnByDefault P.Console)       Console.sidePanel
        -- , SP.button 2 HC.sidePanel
        , SP.button 3 (isOnByDefault P.WsServer)      WS.sidePanel
        -- -- , PaletteList.component 125 2 30.0 96.0
        , StatusLine.component
        , SP.panel (isOnByDefault P.Documentation /\ []) Doc.sidePanel
        , SP.panel (isOnByDefault P.Commands      /\ initialCommands) CL.sidePanel
        , SP.panel (isOnByDefault P.Console       /\ []) Console.sidePanel
        , SP.panel (isOnByDefault P.WsServer      /\ []) WS.sidePanel
        , SP.panel (isOnByDefault P.Tree          /\ []) Tree.sidePanel
        , CommandInput.component
        -- , SP.panel HC.sidePanel
        ]
        )

        $ \_ -> do
            PatchesListbar.selectPatch 1
            -- Key.library >~ Box.focus
            Key.mainScreen >~ Screen.render
    where
        initialCommands = CState.formatHistory initialState
        isOnByDefault which = CState.isPanelOn which initialState