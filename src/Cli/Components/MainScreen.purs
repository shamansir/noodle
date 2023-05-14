module Cli.Components.MainScreen where

import Prelude

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
import Noodle.Network2 as Network
import Noodle.Toolkit3 as Toolkit

import Cli.Keys (mainScreen, nodeList) as Key
import Cli.State.NwWraper (unwrapN)
import Cli.State (State)
import Cli.State (initial) as State

import Cli.Components.PatchBox as PatchBox
import Cli.Components.PatchesBar as PatchesBar
import Cli.Components.AddPatch as AddPatch

import Toolkit.Hydra2 (toolkit) as Hydra


families :: Array Id.FamilyR
families = List.toUnfoldable $ Toolkit.nodeFamilies Hydra.toolkit


component ∷ Core.Blessed State
component =
    B.screenAnd Key.mainScreen

        [ Screen.title "Noodle"
        , Screen.smartCSR true
        , Screen.fullUnicode true
        , Screen.key
            [ Key.escape, Key.alpha 'q', (Key.control $ Key.alpha 'C') ]
            $ \_ kevt -> do
                Blessed.exit
        ]

        [ PatchesBar.component $ Network.patches $ unwrapN State.initial.network
        , PatchBox.component families
        , AddPatch.component
        ]


        $ \_ -> do
            PatchesBar.selectPatch 1
            Key.nodeList >~ Box.focus
            Key.mainScreen >~ Screen.render
