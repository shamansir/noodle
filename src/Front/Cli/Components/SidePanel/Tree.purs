module Cli.Components.SidePanel.Tree where

import Prelude

import Data.Array (singleton) as Array
import Data.Tuple.Nested ((/\))

import Cli.State (State)
import Cli.State (togglePanel, isPanelOn) as CState
import Cli.Components.SidePanel (SidePanel)
-- import Cli.Components.SidePanel as SidePanel
import Cli.Keys as Key
import Front.Shared.Panels (Which(..)) as P

import Noodle.Tree (formatPathTree, toPathTree)


sidePanel :: forall tk p fs sr cr m. SidePanel "tree" (State _ tk p fs sr cr m) Boolean
sidePanel =
    { title : "tree"
    , char : const 'T'
    , isOn : identity
    , panelKey  : Key.treeBox
    , buttonKey : Key.treeButton
    , next : (\s ->
            toPathTree (const unit) s.network
            <#> \pathTree ->
                CState.isPanelOn P.Tree s /\ Array.singleton (formatPathTree pathTree)
        )
    , onToggle : CState.togglePanel P.Tree
    }