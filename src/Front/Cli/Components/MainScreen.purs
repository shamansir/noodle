module Cli.Components.MainScreen where

import Prelude

import Effect (Effect)

import Data.List (toUnfoldable) as List

import Blessed ((>~))
import Blessed as B
import Blessed (exit) as Blessed

import Blessed.Internal.Core as Core
import Blessed.Core.Key (alpha, control, escape) as Key

import Blessed.UI.Base.Screen.Event (key) as Screen
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Base.Screen.Option as Screen
import Blessed.UI.Boxes.Box.Method as Box

import Noodle.Id as Id
import Noodle.Network as Network
import Noodle.Toolkit as Toolkit
import Noodle.Wiring (class Wiring)
import Noodle.Repr (class HasFallback)
import Noodle.Fn.ToFn (class PossiblyToFn)

import Noodle.Ui.Cli.Tagging.At as T
import Noodle.Ui.Cli.Tagging.At (ChannelLabel) as At

import Cli.Keys (mainScreen, library) as Key
import Cli.State (State)
import Cli.State (families) as State
import Cli.Class.CliFriendly (class CliFriendly)


import Cli.Components.PatchBox as PatchBox
import Cli.Components.PatchesListbar as PatchesListbar
import Cli.Components.AddPatchButton as AddPatchButton
-- import Cli.Components.LoadFileButton as LoadFileButton
-- import Cli.Components.CommandLogButton as CommandLogButton
-- import Cli.Components.HydraCodeButton as HydraCodeButton
-- import Cli.Components.FullInfoButton as FullInfoButton
-- import Cli.Components.PaletteList as PaletteList
import Cli.Components.StatusLine as StatusLine
-- import Cli.Components.CommandLogBox as CommandLogBox
-- import Cli.Components.HydraCodeBox as HydraCodeBox
-- import Cli.Components.FullInfoBox as FullInfoBox
-- import Cli.Components.WsStatusButton as WsStatusButton


-- import Toolkit.Hydra (toolkit, Toolkit) as Hydra


-- families :: Array Id.FamilyR
-- families = List.toUnfoldable $ Toolkit.nodeFamilies (Hydra.toolkit :: Hydra.Toolkit Effect)


-- TODO: take toolkit here
component
    :: forall tk p fs repr
    .  HasFallback repr
    => Toolkit.HoldsFamilies repr Effect fs
    => PossiblyToFn tk repr repr Id.FamilyR
    => CliFriendly tk fs repr Effect
    => State tk p fs repr Effect
    -> Core.Blessed (State tk p fs repr Effect)
component initialState =
    B.screenAnd Key.mainScreen

        [ Screen.title "Noodle"
        , Screen.smartCSR true
        , Screen.fullUnicode true
        , Screen.key
            [ Key.escape, Key.alpha 'q', (Key.control $ Key.alpha 'C') ]
            $ \_ kevt -> do
                Blessed.exit
        ]

        [ PatchesListbar.component $ Network.patches $ initialState.network
        , AddPatchButton.component
        , PatchBox.component $ Network.toolkit initialState.network

        -- , LoadFileButton.component
        -- , CommandLogButton.component
        -- , HydraCodeButton.component
        -- , FullInfoButton.component
        -- , WsStatusButton.component
        -- -- , PaletteList.component 125 2 30.0 96.0
        , StatusLine.component
        -- , CommandLogBox.component
        -- , HydraCodeBox.component
        -- , FullInfoBox.component
        ]

        $ \_ -> do
            PatchesListbar.selectPatch 1
            -- Key.library >~ Box.focus
            Key.mainScreen >~ Screen.render
